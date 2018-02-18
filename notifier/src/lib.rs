extern crate notify;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use notify::*;
use notify::DebouncedEvent::*;
use std::sync::mpsc::channel;
use std::time::Duration;
use std::os::raw::c_int;
#[no_mangle]
pub extern "C" fn watch_for_changes(
    path_ptr: *const c_char,
    cb: extern "C" fn(event_for_path: *const c_char) -> c_int,
) {
    unsafe {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(1)).unwrap();
        let path = CStr::from_ptr(path_ptr).to_str().expect("Invalid path");
        watcher.watch(path, RecursiveMode::Recursive).unwrap();

        loop {
            match rx.recv() {
                Ok(event) => {
                    if let Some(e) = event_for(event) {
                        cb(e.as_ptr());
                    }
                }
                Err(e) => println!("watch error: {:?}", e),
            };
        }
    }
}

fn event_for(event: DebouncedEvent) -> Option<CString> {
    let maybe_path = match event {
        NoticeWrite(path) => path.to_str().map(|p| p.to_string()),
        NoticeRemove(path) => path.to_str().map(|p| p.to_string()),
        Create(path) => path.to_str().map(|p| p.to_string()),
        Write(path) => path.to_str().map(|p| p.to_string()),
        Chmod(path) => path.to_str().map(|p| p.to_string()),
        Remove(path) => path.to_str().map(|p| p.to_string()),
        Rename(_, to) => to.to_str().map(|p| p.to_string()),
        Rescan => None,
        Error(_, None) => None,
        Error(_, Some(path)) => path.to_str().map(|p| p.to_string()),
    };
    maybe_path.map(|p| CString::new(p).unwrap())
}
