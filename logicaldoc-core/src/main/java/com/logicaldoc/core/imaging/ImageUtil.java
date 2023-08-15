package com.logicaldoc.core.imaging;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorModel;
import java.awt.image.LookupOp;
import java.awt.image.ShortLookupTable;
import java.awt.image.WritableRaster;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.util.GhostUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * Utility methods for handling images
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.5.3
 */
public class ImageUtil {

	protected static Logger log = LoggerFactory.getLogger(ImageUtil.class);

	private ImageUtil() {
	}

	/**
	 * Saves an image into a given file in an image format
	 * 
	 * @param content the content of the image Base64 encoded
	 * @param file the output file
	 * 
	 * @throws IOException in case of I/O error
	 */
	public static void saveImage(String content, File file) throws IOException {
		BufferedImage image = null;
		byte[] imageByte = Base64.decodeBase64(content);
		ByteArrayInputStream bis = new ByteArrayInputStream(imageByte);
		image = ImageIO.read(bis);
		bis.close();

		// write the image to a file
		ImageIO.write(image, file.getName().contains(".") ? FileUtil.getExtension(file.getName()).toLowerCase() : "jpg",
				file);
	}

	/**
	 * Crops an area and stores it in a given .jpg file
	 * 
	 * @param content the content of the source image Base64 encoded
	 * @param zone the zone definition
	 * @param file the output file
	 * 
	 * @throws IOException an I/O error
	 */
	public static void cropImageToFile(String content, ImageZone zone, File file) throws IOException {
		File tmpFile = null;
		try {
			tmpFile = FileUtil.createTempFile("zonalocr", ".jpg");
			saveImage(content, tmpFile);
			cropImageToFile(tmpFile, zone, file);
		} finally {
			FileUtil.strongDelete(tmpFile);
		}
	}

	/**
	 * Crops an area and stores it in a given .jpg file
	 * 
	 * @param originalFile The original image to croop
	 * @param zone the zone definition
	 * @param file the output file
	 * 
	 * @throws IOException an I/O error
	 */
	private static void cropImageToFile(File originalFile, ImageZone zone, File file) throws IOException {
		BufferedImage scanImage = ImageIO.read(originalFile);
		BufferedImage zoneImage = ImageUtil.cropImage(scanImage, zone);
		// write the zone image to a file
		ImageIO.write(zoneImage, "jpg", file);
	}

	/**
	 * Crops an area from given image
	 * 
	 * @param originalImage the original image
	 * @param zone the zone definition
	 * 
	 * @return the cropped image
	 */
	public static BufferedImage cropImage(BufferedImage originalImage, ImageZone zone) {
		int width = originalImage.getWidth();
		int height = originalImage.getHeight();

		int x = (int) (zone.getLeft() * width);
		int y = (int) (zone.getTop() * height);
		int w = (int) (zone.getWidth() * width);
		int h = (int) (zone.getHeight() * height);

		return originalImage.getSubimage(x, y, w, h);
	}

	/**
	 * Crops a square at the center of the given image
	 * 
	 * @param originalImage the original image
	 * @param squareSize size of the square
	 * 
	 * @return the cropped square
	 */
	public static BufferedImage cropCenterSquare(BufferedImage originalImage, int squareSize) {
		int width = originalImage.getWidth();
		int height = originalImage.getHeight();

		int x = width < squareSize ? 0 : (int) (width / 2D) - (int) (squareSize / 2D);
		int y = height < squareSize ? 0 : (int) (height / 2D) - (int) (squareSize / 2D);
		int w = width < squareSize ? width : squareSize;
		int h = height < squareSize ? height : squareSize;

		return originalImage.getSubimage(x, y, w, h);
	}

	/**
	 * Crops an image leaving the smallest square containing something visible
	 * 
	 * @param src the source image file
	 * @param dst the cropped image file (png format)
	 * 
	 * @return width and height of the cropped image
	 * 
	 * @throws IOException In case of elaboration error
	 */
	public static int[] cropVisibleContent(File src, File dst) throws IOException {
		BufferedImage srcImage = ImageIO.read(src);

		int top = getVisibleTop(srcImage);

		int bottom = getVisibleBottom(srcImage);

		int left = getVisibleLeft(srcImage);

		int right = getVisibleRight(srcImage);

		BufferedImage croppedImage = srcImage.getSubimage(left - 1, top - 1, right - left + 2, bottom - top + 2);
		ImageIO.write(croppedImage, "png", dst);
		return new int[] { croppedImage.getWidth(), croppedImage.getHeight() };
	}

	private static int getVisibleRight(BufferedImage srcImage) {
		int right = 1;
		for (int x = 0; x < srcImage.getWidth(); x++) {
			for (int y = 1; y < srcImage.getHeight(); y++) {
				Color pixelColor = new Color(srcImage.getRGB(x, y));
				if (!pixelColor.equals(Color.WHITE) && x > right) {
					right = x;
					break;
				}
			}
		}
		return right;
	}

	private static int getVisibleLeft(BufferedImage srcImage) {
		int left = srcImage.getWidth() - 1;
		for (int x = srcImage.getWidth() - 1; x > 0; x--) {
			for (int y = 1; y < srcImage.getHeight(); y++) {
				Color pixelColor = new Color(srcImage.getRGB(x, y));
				if (!pixelColor.equals(Color.WHITE) && x < left) {
					left = x;
					break;
				}
			}
		}
		return left;
	}

	private static int getVisibleBottom(BufferedImage srcImage) {
		int bottom = 0;
		for (int x = 0; x < srcImage.getWidth(); x++) {
			for (int y = 0; y < srcImage.getHeight(); y++) {
				Color pixelColor = new Color(srcImage.getRGB(x, y));
				if (!pixelColor.equals(Color.WHITE) && y > bottom) {
					bottom = y;
					break;
				}
			}
		}
		return bottom;
	}

	private static int getVisibleTop(BufferedImage srcImage) {
		int top = srcImage.getHeight() - 1;
		for (int x = 1; x < srcImage.getWidth(); x++) {
			for (int y = srcImage.getHeight() - 1; y > 0; y--) {
				Color pixelColor = new Color(srcImage.getRGB(x, y));
				if (!pixelColor.equals(Color.WHITE) && y < top) {
					top = y;
					break;
				}
			}
		}
		return top;
	}

	/**
	 * Prints the contents of imageIn on container the given opaque value
	 * 
	 * @param container the bigger image
	 * @param imageIn the image to print in
	 * @param opaque and indication of how opaque must be the print
	 * @param x left coordinate
	 * @param y top coordinate
	 */
	public static void pasteImage(BufferedImage container, BufferedImage imageIn, float opaque, int x, int y) {
		Graphics2D g2d = container.createGraphics();
		g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opaque));
		g2d.drawImage(imageIn, x, y, null);
		g2d.dispose();
	}

	/**
	 * Encodes in Base64 the given image file
	 * 
	 * @param image the image to encode
	 * 
	 * @return the encoded image
	 * 
	 * @throws IOException error in the elaboration of the image
	 */
	public static String encodeImage(File image) throws IOException {
		byte[] bytes = IOUtils.toByteArray(new FileInputStream(image));
		return Base64.encodeBase64String(bytes);
	}

	/**
	 * Prints the first page of the given file into the output as .jpg image
	 * 
	 * @param originalFile the original file
	 * @param originalFileName the original filename
	 * @param out the output .jpg file
	 * 
	 * @throws IOException a generic I/O error
	 */
	public static void printFirstPage(File originalFile, String originalFileName, File out) throws IOException {
		/*
		 * In case of an image, just use the original document's file
		 */
		String ext = FileUtil.getExtension(originalFileName).toLowerCase();

		if (ext.equals("jpg") || ext.equals("jpeg")) {
			FileUtil.copyFile(originalFile, out);
		} else {
			File pdfFile = FileUtil.createTempFile("zonalocr", ".pdf");
			try {
				if (!"pdf".equals(ext)) {
					FormatConverterManager manager = (FormatConverterManager) Context.get()
							.getBean(FormatConverterManager.class);
					manager.convertFile(originalFile, originalFileName, pdfFile, "jpg", null);
				} else {
					pdfFile = originalFile;
				}
				GhostUtil.print(pdfFile, out, 1);
			} finally {
				FileUtil.strongDelete(pdfFile);
			}
		}
	}

	/**
	 * Whether the pixel is black.
	 * 
	 * @param image source image
	 * @param x x coordinate
	 * @param y y coordinate
	 * @return true only if the image is black
	 */
	public static boolean isBlack(BufferedImage image, int x, int y) {
		if (image.getType() == BufferedImage.TYPE_BYTE_BINARY) {
			WritableRaster raster = image.getRaster();
			int pixelRGBValue = raster.getSample(x, y, 0);
			return pixelRGBValue == 0;
		}

		int luminanceValue = 140;
		return isBlack(image, x, y, luminanceValue);
	}

	/**
	 * Whether the pixel is black.
	 * 
	 * @param image source image
	 * @param x x coordinate
	 * @param y y coordinate
	 * @param luminanceCutOff cut off luminance
	 * @return true only if the image is black
	 */
	public static boolean isBlack(BufferedImage image, int x, int y, int luminanceCutOff) {
		int pixelRGBValue;
		int r;
		int g;
		int b;
		double luminance = 0.0;

		// return white on areas outside of image boundaries
		if (x < 0 || y < 0 || x > image.getWidth() || y > image.getHeight()) {
			return false;
		}

		try {
			pixelRGBValue = image.getRGB(x, y);
			r = (pixelRGBValue >> 16) & 0xff;
			g = (pixelRGBValue >> 8) & 0xff;
			b = (pixelRGBValue) & 0xff;
			luminance = (r * 0.299) + (g * 0.587) + (b * 0.114);
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}

		return luminance < luminanceCutOff;
	}

	/**
	 * Rotates image.
	 * 
	 * @param image source image
	 * @param angle by degrees
	 * @param cx x-coordinate of pivot point
	 * @param cy y-coordinate of pivot point
	 * @return rotated image
	 */
	public static BufferedImage rotate(BufferedImage image, double angle, int cx, int cy) {
		int width = image.getWidth(null);
		int height = image.getHeight(null);

		int minX = 0;
		int minY = 0;
		int maxX = 0;
		int maxY = 0;

		int[] corners = { 0, 0, width, 0, width, height, 0, height };

		double theta = Math.toRadians(angle);
		for (int i = 0; i < corners.length; i += 2) {
			int x = (int) (Math.cos(theta) * (corners[i] - cx) - Math.sin(theta) * (corners[i + 1] - cy) + cx);
			int y = (int) (Math.sin(theta) * (corners[i] - cx) + Math.cos(theta) * (corners[i + 1] - cy) + cy);

			if (x > maxX) {
				maxX = x;
			}

			if (x < minX) {
				minX = x;
			}

			if (y > maxY) {
				maxY = y;
			}

			if (y < minY) {
				minY = y;
			}

		}

		cx = (cx - minX);
		cy = (cy - minY);

		BufferedImage bi = new BufferedImage((maxX - minX), (maxY - minY), image.getType());
		Graphics2D g2 = bi.createGraphics();
		g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);

		g2.setBackground(Color.white);
		g2.fillRect(0, 0, bi.getWidth(), bi.getHeight());

		AffineTransform at = new AffineTransform();
		at.rotate(theta, cx, cy);

		g2.setTransform(at);
		g2.drawImage(image, -minX, -minY, null);
		g2.dispose();

		return bi;
	}

	/**
	 * Convenience method that returns a scaled instance of the provided
	 * {@code BufferedImage}.
	 *
	 * @param image the original image to be scaled
	 * @param targetWidth the desired width of the scaled instance, in pixels
	 * @param targetHeight the desired height of the scaled instance, in pixels
	 * @return a scaled version of the original {@code BufferedImage}
	 */
	public static BufferedImage getScaledInstance(BufferedImage image, int targetWidth, int targetHeight) {
		int type = (image.getTransparency() == Transparency.OPAQUE) ? BufferedImage.TYPE_INT_RGB
				: BufferedImage.TYPE_INT_ARGB;
		BufferedImage tmp = new BufferedImage(targetWidth, targetHeight, type);
		Graphics2D g2 = tmp.createGraphics();
		g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
		g2.drawImage(image, 0, 0, targetWidth, targetHeight, null);
		g2.dispose();
		return tmp;
	}

	/**
	 * Convenience method that returns a scaled instance of the provided
	 * {@code IIOImage}.
	 *
	 * @param iioSource the original image to be scaled
	 * @param scale the desired scale
	 * @return a scaled version of the original {@code IIOImage}
	 */
	public static IIOImage getScaledInstance(IIOImage iioSource, float scale) {
		if (!(iioSource.getRenderedImage() instanceof BufferedImage)) {
			throw new IllegalArgumentException("RenderedImage in IIOImage must be BufferedImage");
		}

		if (Math.abs(scale - 1.0) < 0.001) {
			return iioSource;
		}

		BufferedImage source = (BufferedImage) iioSource.getRenderedImage();
		BufferedImage target = getScaledInstance(source, (int) (scale * source.getWidth()),
				(int) (scale * source.getHeight()));
		return new IIOImage(target, null, null);
	}

	/**
	 * A replacement for the standard <code>BufferedImage.getSubimage</code>
	 * method.
	 *
	 * @param image the image to take the sub area from
	 * @param x the X coordinate of the upper-left corner of the specified
	 *        rectangular region
	 * @param y the Y coordinate of the upper-left corner of the specified
	 *        rectangular region
	 * @param width the width of the specified rectangular region
	 * @param height the height of the specified rectangular region
	 * @return a BufferedImage that is the subimage of <code>image</code>.
	 */
	public static BufferedImage getSubImage(BufferedImage image, int x, int y, int width, int height) {
		int type = (image.getTransparency() == Transparency.OPAQUE) ? BufferedImage.TYPE_INT_RGB
				: BufferedImage.TYPE_INT_ARGB;
		BufferedImage tmp = new BufferedImage(width, height, type);
		Graphics2D g2 = tmp.createGraphics();
		g2.drawImage(image.getSubimage(x, y, width, height), 0, 0, null);
		g2.dispose();
		return tmp;
	}

	/**
	 * A simple method to convert an image to binary or B/W image.
	 *
	 * @param image input image
	 * @return a monochrome image
	 */
	public static BufferedImage convertImageToBinary(BufferedImage image) {
		BufferedImage tmp = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_BYTE_BINARY);
		Graphics2D g2 = tmp.createGraphics();
		g2.drawImage(image, 0, 0, null);
		g2.dispose();
		return tmp;
	}

	/**
	 * A simple method to convert an image to gray scale.
	 *
	 * @param image input image
	 * @return a monochrome image
	 */
	public static BufferedImage convertImageToGrayscale(BufferedImage image) {
		BufferedImage tmp = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_BYTE_GRAY);
		Graphics2D g2 = tmp.createGraphics();
		g2.drawImage(image, 0, 0, null);
		g2.dispose();
		return tmp;
	}

	private static final short[] invertTable;

	static {
		invertTable = new short[256];
		for (int i = 0; i < 256; i++) {
			invertTable[i] = (short) (255 - i);
		}
	}

	/**
	 * Inverts image color.
	 *
	 * @param image input image
	 * @return an inverted-color image
	 */
	public static BufferedImage invertImageColor(BufferedImage image) {
		BufferedImage tmp = new BufferedImage(image.getWidth(), image.getHeight(), image.getType());
		BufferedImageOp invertOp = new LookupOp(new ShortLookupTable(0, invertTable), null);
		return invertOp.filter(image, tmp);
	}

	/**
	 * Rotates an image.
	 *
	 * @param image the original image
	 * @param angle the degree of rotation
	 * @return a rotated image
	 */
	public static BufferedImage rotateImage(BufferedImage image, double angle) {
		double theta = Math.toRadians(angle);
		double sin = Math.abs(Math.sin(theta));
		double cos = Math.abs(Math.cos(theta));
		int w = image.getWidth();
		int h = image.getHeight();
		int newW = (int) Math.floor(w * cos + h * sin);
		int newH = (int) Math.floor(h * cos + w * sin);

		BufferedImage tmp = new BufferedImage(newW, newH, image.getType());
		Graphics2D g2d = tmp.createGraphics();
		g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
		g2d.translate((newW - w) / 2D, (newH - h) / 2D);
		g2d.rotate(theta, w / 2D, h / 2D);
		g2d.drawImage(image, 0, 0, null);
		g2d.dispose();
		return tmp;
	}

	/**
	 * Gets an image from Clipboard.
	 *
	 * @return image the image to process
	 */
	public static Image getClipboardImage() {
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		try {
			return (Image) clipboard.getData(DataFlavor.imageFlavor);
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Clones an image.
	 *
	 * @param bi the image to clone
	 * @return the cloned image
	 */
	public static BufferedImage cloneImage(BufferedImage bi) {
		ColorModel cm = bi.getColorModel();
		boolean isAlphaPremultiplied = cm.isAlphaPremultiplied();
		WritableRaster raster = bi.copyData(null);
		return new BufferedImage(cm, raster, isAlphaPremultiplied, null);
	}

	/**
	 * Creates an images with the givent text printed inside
	 * 
	 * @param text the text to print in the image
	 * @return The generated image
	 */
	public static BufferedImage textToImage(String text) {
		BufferedImage image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);

		Graphics2D graphics2d = image.createGraphics();
		Font font = new Font("TimesNewRoman", Font.BOLD, 32);
		graphics2d.setFont(font);
		FontMetrics fontmetrics = graphics2d.getFontMetrics();
		int width = fontmetrics.stringWidth(text);
		int height = fontmetrics.getHeight();
		graphics2d.dispose();

		image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		graphics2d = image.createGraphics();
		graphics2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION,
				RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
		graphics2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY);
		graphics2d.setFont(font);
		fontmetrics = graphics2d.getFontMetrics();
		graphics2d.setColor(Color.DARK_GRAY);
		graphics2d.drawString(text, 0, fontmetrics.getAscent());
		graphics2d.dispose();

		return image;
	}
}