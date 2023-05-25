package com.logicaldoc.core.util;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDResources;
import org.apache.pdfbox.pdmodel.graphics.PDXObject;
import org.apache.pdfbox.pdmodel.graphics.form.PDFormXObject;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This utility class allows the extraction of raster images from a PDF document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0.0
 */
public class PDFImageExtractor {

	protected static Logger log = LoggerFactory.getLogger(PDFImageExtractor.class);

	private PDDocument document = null;

	/**
	 * Creates a new PDF reader for the given PDF file
	 * 
	 * @param pdfFile the pdf file
	 */
	public PDFImageExtractor(File pdfFile) {
		try {
			document = PDDocument.load(pdfFile);
		} catch (IOException e) {
			log.warn(e.getMessage());
		}
	}

	/**
	 * Closes the PDF and releases resources used
	 * 
	 * @throws java.io.IOException if the pdf file cannot be read
	 */
	public void close() throws java.io.IOException {
		if (document != null) {
			document.close();
			document = null;
		}
	}

	/**
	 * Returns the total number of pages in the PDF
	 * 
	 * @return the total number of pages
	 */
	public int getNumberOfPages() {
		try {
			return document.getPages().getCount();
		} catch (Exception t) {
			log.warn(t.getMessage());
			return 1;
		}
	}

	/**
	 * Renders the specified page as a buffered image
	 * 
	 * @param pageIndex zero based page index, i.e., the first page is page 0
	 * 
	 * @return object representation of the image
	 * 
	 * @throws IOException if the pdf file cannot be read
	 */
	public BufferedImage getPageAsImage(int pageIndex) throws IOException {
		PDFRenderer pdfRenderer = new PDFRenderer(document);

		// note that the page number parameter is zero based
		return pdfRenderer.renderImageWithDPI(pageIndex, 300, ImageType.RGB);
	}

	/**
	 * Extracts the imageKey image from the given page
	 * 
	 * @param pageIndex zero based page index, i.e., the first page is page 0
	 * @param imageKey identifier of the image
	 * 
	 * @return object representation of the image
	 * 
	 * @throws IOException if the pdf file cannot be read
	 */
	public BufferedImage extractImage(int pageIndex, COSName imageKey) throws IOException {
		return extractImage(document.getPages().get(pageIndex), imageKey);
	}

	private BufferedImage extractImage(PDPage page, COSName imageKey) throws IOException {
		PDResources resources = page.getResources();
		Map<COSName, PDImageXObject> images = getImagesFromResources(resources);
		PDImageXObject image = images.get(imageKey);

		BufferedImage image2 = null;
		try {
			// sometimes the images are rotated in the PDF
			BufferedImage bi = image.getImage();

			if (bi == null) {
				throw new IOException("Error image is null");
			}

			image2 = correctRotation(bi, page.getRotation());
		} catch (Exception e) {
			log.warn("Unable to read: {} bits: {} Suffix: {}", image.getClass(), image.getBitsPerComponent(),
					image.getSuffix());
		}

		// Clear references
		image = null;
		images = null;
		resources = null;

		return image2;
	}

	/**
	 * This utility method correct the image orientation by replicating te page
	 * rotation
	 * 
	 * @param bi the image to correct
	 * @param pageRotation page rotation expressed in degrees (only multiples of
	 *        90 are accepted)
	 * 
	 * @return the modified image
	 */
	private BufferedImage correctRotation(BufferedImage bi, int pageRotation) {
		if (pageRotation == 0)
			return bi;

		BufferedImage rotatedImage = bi;
		for (int i = 0; i < pageRotation; i += 90)
			rotatedImage = rotate90DX(rotatedImage);
		return rotatedImage;
	}

	private BufferedImage rotate90DX(BufferedImage bi) {
		int width = bi.getWidth();
		int height = bi.getHeight();

		BufferedImage biFlip = new BufferedImage(height, width, bi.getType());

		for (int i = 0; i < width; i++)
			for (int j = 0; j < height; j++)
				biFlip.setRGB(height - 1 - j, i, bi.getRGB(i, j));

		return biFlip;
	}

	public BufferedImage rotate90SX(BufferedImage bi) {
		int width = bi.getWidth();
		int height = bi.getHeight();

		BufferedImage biFlip = new BufferedImage(height, width, bi.getType());

		for (int i = 0; i < width; i++)
			for (int j = 0; j < height; j++)
				biFlip.setRGB(j, width - 1 - i, bi.getRGB(i, j));

		return biFlip;
	}

	/**
	 * Gets the set of images identifiers inside the given page
	 * 
	 * @param pageIndex zero based page index, i.e., the first page is page 0
	 * 
	 * @return set of image identifiers
	 * 
	 * @throws IOException if the pdf file cannot be read
	 */
	public Set<COSName> getImageKeys(int pageIndex) throws IOException {
		PDResources resources = document.getPages().get(pageIndex).getResources();
		return getImagesFromResources(resources).keySet();
	}

	private Map<COSName, PDImageXObject> getImagesFromResources(PDResources resources) throws IOException {
		Map<COSName, PDImageXObject> images = new HashMap<>();

		for (COSName xObjectName : resources.getXObjectNames()) {
			PDXObject xObject = resources.getXObject(xObjectName);

			if (xObject instanceof PDFormXObject) {
				images.putAll(getImagesFromResources(((PDFormXObject) xObject).getResources()));
			} else if (xObject instanceof PDImageXObject) {
				images.put(xObjectName, ((PDImageXObject) xObject));
			}
		}

		return images;
	}

	/**
	 * Extracts all images of the entire document
	 * 
	 * @return The list of images
	 * 
	 * @throws IOException if the pdf file cannot be read
	 */
	public List<BufferedImage> extractImages() throws IOException {
		List<BufferedImage> imgs = new ArrayList<>();
		Iterator<PDPage> iter = document.getPages().iterator();
		while (iter.hasNext()) {
			try {
				Collection<PDImageXObject> images = getImagesFromResources(iter.next().getResources()).values();
				for (PDImageXObject pdImageXObject : images)
					imgs.add(pdImageXObject.getImage());
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}
		return imgs;
	}
}