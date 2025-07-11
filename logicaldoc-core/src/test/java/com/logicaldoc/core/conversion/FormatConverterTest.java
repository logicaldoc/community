package com.logicaldoc.core.conversion;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link FormatConverter}s
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class FormatConverterTest extends AbstractCoreTestCase {

	private DocumentDAO docDao;

	private Document document;

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		prepareSession("admin", "admin");

		docDao = Context.get(DocumentDAO.class);
		document = docDao.findById(1L);
	}

	@Test
	public void testGhostscriptConverter() throws IOException {
		File src = new File("src/test/resources/small.pdf");
		File dest = FileUtil.createTempFile("test", ".png");
		GhostscriptConverter converter = new GhostscriptConverter();

		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".jpg");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".ps");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".tiff");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testImageConverter() throws IOException {
		File dest = FileUtil.createTempFile("test", ".pdf");

		ImageConverter converter = new ImageConverter();
		try {
			converter.convert(session.getSid(), document, new File("src/test/resources/logo.png"), dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testMarkdownConverter() throws IOException {
		File src = new File("src/test/resources/README.md");
		File dest = FileUtil.createTempFile("test", ".html");

		MarkdownConverter converter = new MarkdownConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".pdf");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testP7MConverter() throws IOException {
		File src = new File("src/test/resources/test.p7m");
		File dest = FileUtil.createTempFile("test", ".pdf");

		P7MConverter converter = new P7MConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testPstConverter() throws IOException {
		File src = new File("src/test/resources/sample.pst");
		File dest = FileUtil.createTempFile("test", ".pdf");

		PstConverter converter = new PstConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testRarConverter() throws IOException {
		File src = new File("src/test/resources/kofax.rar");
		File dest = FileUtil.createTempFile("test", ".pdf");

		RarConverter converter = new RarConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}
	
	@Test
	public void testTarConverter() throws IOException {
		File src = new File("src/test/resources/kofax.tar");
		File dest = FileUtil.createTempFile("test", ".pdf");

		TarConverter converter = new TarConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testSevenZipConverter() throws IOException {
		File src = new File("src/test/resources/kofax.7z");
		File dest = FileUtil.createTempFile("test", ".pdf");

		SevenZipConverter converter = new SevenZipConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testZipConverter() throws IOException {
		File src = new File("src/test/resources/test.zip");
		File dest = FileUtil.createTempFile("test", ".pdf");

		ZipConverter converter = new ZipConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}
	
	@Test
	public void testXMLConverter() throws IOException {
		File src = new File("src/test/resources/context.xml");
		File dest = FileUtil.createTempFile("test", ".pdf");

		XMLConverter converter = new XMLConverter();
		try {
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		try {
			dest = FileUtil.createTempFile("test", ".txt");
			converter.convert(session.getSid(), document, src, dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}
}