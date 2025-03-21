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
		assertEquals("image/jpeg", res);	
		
		res = MimeType.getByFilename("dddd.webp");
		assertEquals("image/webp", res);
		
		res = MimeType.getByFilename("dddd.webm");
		assertEquals("video/webm", res);
		
		// extension not defined, should default to "application/octet-stream"
		res = MimeType.getByFilename("Smart Insert.edge");
		assertNotSame("video/webm", res);
		assertEquals("application/octet-stream", res);
		
		res = MimeType.getByFilename("Fwd- R- Preavviso vacanze Ordini aperti.eml");
		assertEquals("message/rfc822", res);
		
		// New checks 14/03/2025
		res = MimeType.getByFilename("OriginalFile.ppt");
		assertEquals("application/vnd.ms-powerpoint", res);
		
		res = MimeType.getByFilename("Business Process Reengineering.doc");
		assertEquals("application/msword", res);
		
		res = MimeType.getByFilename("UNUS.xls");
		assertEquals("application/vnd.ms-excel", res);
		
		res = MimeType.getByFilename("003-005-CT-f0c30c4b-75a1-4fb9-8fa1-4ccbce309869-1543533054430.rtf"); // text/richtext
		assertEquals("application/rtf", res);
		
		// Sources:
		// https://mimetype.io/
		// https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types/Common_types
		// https://www.iana.org/assignments/media-types/media-types.xhtml
		// Python 3.11.4 mimetypes - https://docs.python.org/3/library/mimetypes.html
	}
	
	@Test
	public void testGet() {
		String res = MimeType.get("jfif");
		assertEquals("image/jpeg", res);
		
		res = MimeType.get("docx");
		assertEquals("application/vnd.openxmlformats-officedocument.wordprocessingml.document", res);
		
		// extension not defined, should default to "application/octet-stream"
		res = MimeType.get("trec");
		assertEquals("application/octet-stream", res);
		
		res = MimeType.get(".mp4");
		assertEquals("video/mp4", res);
		
		res = MimeType.get(".eml");
		assertEquals("message/rfc822", res);
		
		res = MimeType.get(".jpg");
		assertEquals("image/jpeg", res);
		
		res = MimeType.get(".jpeg");
		assertEquals("image/jpeg", res);		
	}	

}
