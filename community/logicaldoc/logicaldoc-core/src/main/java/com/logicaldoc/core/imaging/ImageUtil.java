package com.logicaldoc.core.imaging;

import java.awt.AlphaComposite;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FilenameUtils;
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
		ImageIO.write(image,
				file.getName().contains(".") ? FilenameUtils.getExtension(file.getName()).toLowerCase() : "jpg", file);
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
			tmpFile = File.createTempFile("zonalocr", ".jpg");
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

		int x = (int) (zone.getLeft() * (double) width);
		int y = (int) (zone.getTop() * (double) height);
		int w = (int) (zone.getWidth() * (double) width);
		int h = (int) (zone.getHeight() * (double) height);

		BufferedImage subImgage = originalImage.getSubimage(x, y, w, h);
		return subImgage;
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

		int x = width < squareSize ? 0 : (int) ((double) (width / 2)) - (int) ((double) (squareSize / 2));
		int y = height < squareSize ? 0 : (int) ((double) (height / 2)) - (int) ((double) (squareSize / 2));
		int w = width < squareSize ? width : squareSize;
		int h = height < squareSize ? height : squareSize;

		BufferedImage subImgage = originalImage.getSubimage(x, y, w, h);
		return subImgage;
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
	 * @throws FileNotFoundException error accessing the file
	 */
	public static String encodeImage(File image) throws FileNotFoundException, IOException {
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
		String ext = FilenameUtils.getExtension(originalFileName).toLowerCase();

		if (ext.equals("jpg") || ext.equals("jpeg")) {
			FileUtil.copyFile(originalFile, out);
		} else {
			File pdfFile = File.createTempFile("zonalocr", ".pdf");
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
}