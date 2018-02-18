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
                    if let Some(e) = event_for_path(event) {
                        cb(e.as_ptr());
                    }
                }
                Err(e) => println!("watch error: {:?}", e),
            };
        }
    }
}

fn event_for_path(event: DebouncedEvent) -> Option<CString> {
    match event {
        NoticeWrite(path) => Some(path),
        NoticeRemove(path) => Some(path),
        Create(path) => Some(path),
        Write(path) => Some(path),
        Chmod(path) => Some(path),
        Remove(path) => Some(path),
        Rename(_, to) => Some(to),
        Rescan => None,
        Error(_, path) => path,
    }.map(|p| p.to_string_lossy().into_owned())
        .map(|p| CString::new(p).unwrap())
}
