use libz_rs_sys::{
    inflate, inflateEnd, inflateInit2_, inflateMark, z_stream, zlibVersion, Z_NO_FLUSH, Z_OK,
};

#[test]
fn mark_includes_distance_bits_when_match_awaits_output() {
    let mut stream = z_stream::default();
    let mut input = [0x73u8, 0x04, 0x02, 0x00];
    let mut output = [0u8; 2];

    unsafe {
        assert_eq!(
            inflateInit2_(
                &mut stream,
                -15,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as i32,
            ),
            Z_OK
        );
        stream.next_in = input.as_mut_ptr();
        stream.avail_in = input.len() as u32;
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as u32;
        let result = inflate(&mut stream, Z_NO_FLUSH);
        let mark = inflateMark(&stream);
        assert_eq!(inflateEnd(&mut stream), Z_OK);
        assert_eq!(result, Z_OK);
        assert_eq!(output, *b"AA");
        assert_eq!(stream.avail_out, 0);
        assert_eq!(stream.total_out, 2);
        assert_eq!((mark >> 16, mark & 0xffff), (12, 1));
    }
}

#[test]
fn mark_includes_distance_bits_after_input_resume() {
    let mut stream = z_stream::default();
    let mut input = [0x73u8, 0xc4, 0x02, 0x00];
    let mut output = [0u8; 2];

    unsafe {
        assert_eq!(
            inflateInit2_(
                &mut stream,
                -15,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as i32,
            ),
            Z_OK
        );
        stream.next_in = input.as_mut_ptr();
        stream.avail_in = 3;
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as u32;
        assert_eq!(inflate(&mut stream, Z_NO_FLUSH), Z_OK);
        let intermediate = inflateMark(&stream);
        assert_eq!((intermediate >> 16, intermediate & 0xffff), (9, 0));
        assert_eq!(stream.avail_in, 0);
        assert_eq!((stream.total_in, stream.total_out), (3, 1));
        assert_eq!(stream.avail_out, 1);
        stream.next_in = input.as_mut_ptr().add(3);
        stream.avail_in = 1;
        let result = inflate(&mut stream, Z_NO_FLUSH);
        let mark = inflateMark(&stream);
        assert_eq!(inflateEnd(&mut stream), Z_OK);
        assert_eq!(result, Z_OK);
        assert_eq!(output, *b"AA");
        assert_eq!((stream.avail_in, stream.avail_out), (0, 0));
        assert_eq!((stream.total_in, stream.total_out), (4, 2));
        assert_eq!((mark >> 16, mark & 0xffff), (14, 1));
    }
}
