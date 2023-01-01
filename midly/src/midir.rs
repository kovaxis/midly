use crate::prelude::*;
use midir::{
    ConnectError, MidiInput, MidiInputConnection, MidiInputPort, MidiOutputConnection, SendError,
};

pub use midir::*;

pub trait MidirOutBridge {
    fn send_message(&mut self, msg: &MidiMessage) -> Result<(), SendError>;
}
impl MidirOutBridge for MidiOutputConnection {
    fn send_message(&mut self, msg: &MidiMessage) -> Result<(), SendError> {
        let mut buf = [0; 3];
        let bytes = msg.encode(&mut buf);
        if !bytes.is_empty() {
            self.send(bytes)?;
        }
        Ok(())
    }
}

pub trait MidirInBridge {
    fn midly_connect<F>(
        self,
        port: &MidiInputPort,
        port_name: &str,
        on_midi: F,
    ) -> Result<MidiInputConnection<()>, ConnectError<MidiInput>>
    where
        F: FnMut(u64, MidiMessage) + Send + 'static;
}
impl MidirInBridge for MidiInput {
    fn midly_connect<F>(
        self,
        port: &MidiInputPort,
        port_name: &str,
        mut send_to: F,
    ) -> Result<MidiInputConnection<()>, ConnectError<MidiInput>>
    where
        F: FnMut(u64, MidiMessage) + Send + 'static,
    {
        self.connect(
            port,
            port_name,
            move |timestamp, raw, _| {
                let msg = MidiMessage::decode(raw);
                send_to(timestamp, msg);
            },
            (),
        )
    }
}
