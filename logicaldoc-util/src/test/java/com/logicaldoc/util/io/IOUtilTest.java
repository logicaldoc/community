package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;

import org.junit.Test;

public class IOUtilTest {

	@Test
	public void testGetLimitedStream() throws IOException {
		File file = new File("target/test.txt");
		try (InputStream is = IOUtil.getLimitedStream(this.getClass().getResourceAsStream("/context.properties"), 64)) {
			FileUtil.writeFile(is, file.getPath());
			String content = FileUtil.readFile(file);
			assertTrue(content.startsWith("default.tag.minsize=3"));
			assertTrue(content.endsWith("store.1.dir=targe"));
			assertFalse(content.contains("default.password.occurrence"));
		} finally {
			FileUtil.delete(file);
		}
	}

	@Test
	public void testWrite() throws IOException {
		File file = new File("target/test.txt");
		InputStream is = this.getClass().getResourceAsStream("/context.properties");
		try (OutputStream os = new FileOutputStream(file)) {
			IOUtil.write(is, os);
			assertEquals(new File("src/test/resources/context.properties").length(), file.length());
		} finally {
			IOUtil.close(is);
			FileUtil.delete(file);
		}

		assertFalse(file.exists());
		is = this.getClass().getResourceAsStream("/context.properties");
		try {
			IOUtil.write(is, file);
			assertEquals(new File("src/test/resources/context.properties").length(), file.length());
		} finally {
			IOUtil.close(is);
			FileUtil.delete(file);
		}
	}

	@Test
	public void testGetReadStream() throws IOException {
		try (InputStream is = this.getClass().getResourceAsStream("/context.properties")) {
			String content = IOUtil.readStream(is);
			assertEquals(FileUtil.readFile("src/test/resources/context.properties"), content);
		}
	}

	@Test
	public void testGetBytesOfStream() throws IOException {
		try (InputStream is = this.getClass().getResourceAsStream("/context.properties")) {
			byte[] content = IOUtil.getBytesOfStream(is);
			String str = new String(content);
			assertTrue(str.startsWith("default.tag.minsize=3"));
		}
	}

	@Test
	public void testSerialize() throws IOException {
		HashMap<String, Object> map = new HashMap<>();
		map.put("1", "value1");

		String serialized = IOUtil.serialize(map, null);
		
		@SuppressWarnings("unchecked")
		HashMap<String, Object> deserialized = (HashMap<String, Object>) IOUtil.deserialize(serialized);
		assertNotNull(deserialized);
		assertEquals("value1", deserialized.get("1"));
	}
}