package com.logicaldoc.core.imaging;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for {@link ImageUtil}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ImageUtilTest extends AbstractCoreTestCase {

	private final static String LOGO_IMAGE = "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";

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Test
	public void testSave() throws IOException {
		File out = new File("target/out.png");
		try {
			ImageUtil.save(LOGO_IMAGE, out);

			assertEquals(this.getClass().getResource("/logo.png").openConnection().getContentLength(), out.length());
		} finally {
			FileUtil.delete(out);
		}
	}

	@Test
	public void testCropToFile() throws IOException {
		File out = new File("target/zone.png");
		try {
			ImageZone zone = new ImageZone();
			zone.setWidth(0.5);
			zone.setHeight(0.5);
			ImageUtil.cropToFile(LOGO_IMAGE, zone, out);

			assertTrue(this.getClass().getResource("/logo.png").openConnection().getContentLength() > out.length());
		} finally {
			FileUtil.delete(out);
		}
	}

	@Test
	public void testCropVisibleContent() throws IOException {
		File src = new File("src/test/resources/logo2.png");
		File dest = new File("target/visible.png");
		try {
			int[] dimensions = ImageUtil.cropVisibleContent(src, dest);
			assertEquals(200, dimensions[0]);
			assertEquals(41, dimensions[1]);
			assertTrue(src.length() > dest.length());
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testCropCenterSquare() {
		BufferedImage square = ImageUtil.cropCenterSquare(ImageUtil.decode(LOGO_IMAGE), 50);
		assertEquals(50, square.getWidth());
		assertEquals(42, square.getHeight());
	}

	@Test
	public void testPaste() throws IOException {
		BufferedImage container = ImageUtil.decode(ImageUtil.encode(new File("src/test/resources/logo2.png")));
		byte[] contentBefore = ImageUtil.getBytes(container);
		ImageUtil.paste(container, ImageUtil.decode(LOGO_IMAGE), 0.3F, 10, 10);
		byte[] contentAfter = ImageUtil.getBytes(container);

		assertFalse(contentAfter.length == contentBefore.length);
	}

	@Test
	public void testPrintFirstPage() throws IOException {
		File src = new File("src/test/resources/logo.png");
		File dest = new File("target/out.pdf");
		try {
			ImageUtil.printFirstPage(src, "logo.png", dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}

		dest = new File("target/out.jpg");
		try {
			ImageUtil.printFirstPage(src, "logo.png", dest);
			assertTrue(dest.length() > 0);
		} finally {
			FileUtil.delete(dest);
		}
	}

	@Test
	public void testIsBlack() {
		BufferedImage image = ImageUtil.decode(LOGO_IMAGE);
		assertTrue(ImageUtil.isBlack(image, 1, 1));
		assertTrue(ImageUtil.isBlack(image, 36, 36));

		BufferedImage binary = ImageUtil.convertToBinary(image);
		assertTrue(ImageUtil.isBlack(binary, 1, 1));
		assertTrue(ImageUtil.isBlack(binary, 36, 36));

		BufferedImage gray = ImageUtil.convertToGrayscale(image);
		assertTrue(ImageUtil.isBlack(gray, 1, 1));
		assertTrue(ImageUtil.isBlack(gray, 36, 36));
		
		BufferedImage inverted = ImageUtil.invertColors(image);
		assertFalse(ImageUtil.isBlack(inverted, 1, 1));
		assertFalse(ImageUtil.isBlack(inverted, 36, 36));
	}

	@Test
	public void testRotate() {
		BufferedImage src = ImageUtil.decode(LOGO_IMAGE);
		BufferedImage rotated = ImageUtil.rotate(src, 90);
		assertEquals(src.getWidth(), rotated.getHeight());

		rotated = ImageUtil.rotate(src, 90, 10, 10);
		assertEquals(src.getWidth(), rotated.getHeight());
	}

	@Test
	public void testScale() {
		BufferedImage src = ImageUtil.decode(LOGO_IMAGE);
		BufferedImage scaled = ImageUtil.scale(src, 50, 50);
		assertEquals(50, scaled.getWidth());
		assertEquals(50, scaled.getHeight());
	}

	@Test
	public void testGetSubImage() {
		BufferedImage src = ImageUtil.decode(LOGO_IMAGE);
		BufferedImage sub = ImageUtil.getSubImage(src, 0, 0, 10, 10);
		assertEquals(10, sub.getWidth());
		assertEquals(10, sub.getHeight());
	}
	
	@Test
	public void testClone() {
		BufferedImage image = ImageUtil.decode(LOGO_IMAGE);
		BufferedImage cloned = ImageUtil.clone(image);
		assertEquals(image.getWidth(), cloned.getWidth());
		assertEquals(image.getHeight(), cloned.getHeight());
	}
}