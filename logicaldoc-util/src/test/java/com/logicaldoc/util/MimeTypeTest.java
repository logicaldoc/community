package com.logicaldoc.util;

import static org.junit.Assert.*;

import org.junit.Test;

public class MimeTypeTest {

	@Test
	public void testGetByFilename() {
		String res = MimeType.getByFilename("dddd.heic");
		assertEquals("image/heic", res);
		
		res = MimeType.getByFilename("dddd.msg");
		assertEquals("application/vnd.ms-outlook", res);
		
		res = MimeType.getByFilename("dddd.eml");
		assertEquals("message/rfc822", res);
		
		res = MimeType.getByFilename("dddd.svg");
		assertEquals("image/svg+xml", res);
		
		res = MimeType.getByFilename("dddd.jfif");
		assertEquals("image/pjpeg", res);	
		
		res = MimeType.getByFilename("dddd.webp");
		assertEquals("image/webp", res);
		
		res = MimeType.getByFilename("dddd.webm");
		assertEquals("video/webm", res);
		
		// extension not defined, should default to "application/octet-stream"
		res = MimeType.getByFilename("Smart Insert.edge");
		assertFalse("video/webm".equals(res));
		assertEquals("application/octet-stream", res);
	}
	
	@Test
	public void testGet() {
		String res = MimeType.get("jfif");
		assertEquals("image/pjpeg", res);
		
		res = MimeType.get("docx");
		assertEquals("application/vnd.openxmlformats-officedocument.wordprocessingml.document", res);
		
		// extension not defined, should default to "application/octet-stream"
		res = MimeType.get("trec");
		assertEquals("application/octet-stream", res);
		
		res = MimeType.get(".mp4");
		assertEquals("video/mp4", res);
	}	

}
