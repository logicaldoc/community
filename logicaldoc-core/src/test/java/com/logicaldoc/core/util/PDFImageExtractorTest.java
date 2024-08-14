package com.logicaldoc.core.util;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URLDecoder;
import java.util.List;

import javax.imageio.ImageIO;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class PDFImageExtractorTest {

	File destFolder = null;

	@Before
	public void setUp() {
		destFolder = new File("target", "destFolder");
		if (!destFolder.exists())
			destFolder.mkdir();
	}

	@Test
	public void testGetNumberOfPages() throws IOException {
		String filePath = URLDecoder.decode(getClass().getResource("/exportPDF_OFwImage.pdf").getPath(), "UTF-8");
		File pdffile = new File(filePath);
		PDFImageExtractor pdfReader = new PDFImageExtractor(pdffile);
		int pagesNumb = pdfReader.getNumberOfPages();
		pdfReader.close();
		Assert.assertEquals(1, pagesNumb);
	}

	@Test
	public void testExtactImageOO2_4() throws IOException {
		extractImages("exportPDF_OFwImage");
	}

	public void extractImages(String prefix) throws IOException {
		String filePath = URLDecoder.decode(getClass().getResource("/" + prefix + ".pdf").getPath(), "UTF-8");

		File pdffile = new File(filePath);

		try (PDFImageExtractor pdfReader = new PDFImageExtractor(pdffile);) {
			List<BufferedImage> imgs = pdfReader.extractImages();

			Assert.assertNotNull(imgs);
			Assert.assertTrue(imgs.size() > 0);

			for (int i = 0; i < imgs.size(); i++) {
				File destFile = new File(destFolder, prefix + "_" + i + ".bmp");
				ImageIO.write(imgs.get(i), "bmp", destFile);
			}
		}
	}
}