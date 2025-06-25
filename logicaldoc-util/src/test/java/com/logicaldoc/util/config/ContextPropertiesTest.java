package com.logicaldoc.util.config;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Properties;

import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ContextPropertiesTest {

	@Test
	public void testWrite() throws IOException {
		File abcFile = new File("target/abc1.properties");
		ContextProperties contextProperties = new ContextProperties(abcFile);

		contextProperties.setProperty("test", "value");

		contextProperties.write();

		assertEquals("value", contextProperties.getProperty("test"));
	}

	@Test
	public void testSetAndGetProperty() throws IOException {
		File abcFile = new File("target/abc.properties");
		ContextProperties contextProperties = new ContextProperties(abcFile);

		// testing implicit encoding and decoding in setProperty and getPoperty
		// (ContextProperties class)
		contextProperties.setProperty("propA", "pippo");
		contextProperties.setProperty("propB", """
									pippo
									pluto
									paperino""");
		contextProperties.setProperty("propC",
				"_b64_IExvcmVtIGlwc3VtIGRvbG9yIHNpdCBhbWV0LCBjb25zZWN0ZXR1ciBhZGlwaXNjaW5nIGVsaXQu");

		contextProperties.setProperty("emptyKey", "");

		contextProperties.write();

		assertEquals("pippo", contextProperties.getString("propA"));
		assertEquals("pippo\npluto\npaperino", contextProperties.getString("propB"));
		assertEquals(" Lorem ipsum dolor sit amet, consectetur adipiscing elit.", contextProperties.getString("propC"));
		assertEquals("", contextProperties.getString("emptyKey"));

		Properties properties = new Properties();
		try (FileReader reader = new FileReader(abcFile)) {
			properties.load(reader);
		} finally {
			FileUtil.delete(abcFile);
		}

		// testing explicit encoding and decoding (Properties class)
		assertEquals("pippo", properties.getProperty("propA"));
		assertEquals("_b64_" + Base64.getEncoder().encodeToString("""
									pippo
									pluto
									paperino""".getBytes()), properties.getProperty("propB"));
		assertEquals("_b64_IExvcmVtIGlwc3VtIGRvbG9yIHNpdCBhbWV0LCBjb25zZWN0ZXR1ciBhZGlwaXNjaW5nIGVsaXQu",
				properties.getProperty("propC"));

		String decodedValue = new String(
				Base64.getDecoder()
						.decode("IExvcmVtIGlwc3VtIGRvbG9yIHNpdCBhbWV0LCBjb25zZWN0ZXR1ciBhZGlwaXNjaW5nIGVsaXQu"),
				StandardCharsets.UTF_8);
		assertEquals(" Lorem ipsum dolor sit amet, consectetur adipiscing elit.", decodedValue);

		assertEquals("_b64_cGlwcG8KcGx1dG8KcGFwZXJpbm8=", properties.getProperty("propB"));

		assertEquals("", contextProperties.getProperty("emptyKey"));

		// no encoding in properties.setProperty
		String multiLineValue = """
                pippo
                pluto
                paperino""";
		properties.setProperty("propB", multiLineValue);
		assertEquals(multiLineValue, properties.getProperty("propB"));
	}
}