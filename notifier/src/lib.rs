extern crate notify;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use notify::*;
use notify::DebouncedEvent::*;
use std::sync::mpsc::channel;
use std::time::Duration;
use std::os::raw::c_int;
use std::path::PathBuf;
#[no_mangle]
pub extern "C" fn watch_for_changes(
    path_ptr: *const c_char,
    cb: extern "C" fn(event: *const c_char, a: *const c_char, b: *const c_char) -> c_int,
) {
    unsafe {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(1)).unwrap();
        let path = CStr::from_ptr(path_ptr).to_str().expect("Invalid path");
        watcher.watch(path, RecursiveMode::Recursive).unwrap();

        loop {
            match rx.recv() {
                Ok(event) => {
                    let (event_str, a, b) = event_info_str(event);
                    cb(
                        CString::new(event_str).unwrap().as_ptr(),
                        CString::new(a).unwrap().as_ptr(),
                        CString::new(b).unwrap().as_ptr(),
                    );
                }
                Err(e) => println!("watch error: {:?}", e),
            };
        }
    }
}

fn event_info_str(event: DebouncedEvent) -> (String, String, String) {
    match event {
        NoticeWrite(path) => (
            "NoticeWrite".to_string(),
            String::new(),
            path_to_string(path),
        ),
        NoticeRemove(path) => (
            "NoticeRemove".to_string(),
            String::new(),
            path_to_string(path),
        ),
        Create(path) => ("Create".to_string(), String::new(), path_to_string(path)),
        Write(path) => ("Write".to_string(), String::new(), path_to_string(path)),
        Chmod(path) => ("Chmod".to_string(), String::new(), path_to_string(path)),
        Remove(path) => ("Remove".to_string(), String::new(), path_to_string(path)),
        Rename(from, to) => (
            "Rename".to_string(),
            path_to_string(from),
            path_to_string(to),
        ),
        Rescan => ("Rescan".to_string(), String::new(), String::new()),
        Error(msg, None) => ("Error".to_string(), msg.to_string(), String::new()),
        Error(msg, Some(path)) => ("Error".to_string(), msg.to_string(), path_to_string(path)),
    }
}
fn path_to_string(p: PathBuf) -> String {
    p.to_str().unwrap_or("").to_string()
}
