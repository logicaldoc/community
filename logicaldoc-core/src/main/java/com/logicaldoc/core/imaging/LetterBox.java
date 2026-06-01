package com.logicaldoc.core.imaging;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

/**
 * An image with another image drawn inside at the center and resized to
 * accommodate it within the new dimensions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class LetterBox extends BufferedImage {

    private BufferedImage image;

    private double scale = 1;

    private int offsetX;

    private int offsetY;

    private int originalWidth = 0;

    private int originalHeight = 0;

    /**
     * Constructor
     * 
     * @param src The source image
     * @param targetW The width of the target image
     * @param targetH The height of the target image
     * @param padColor The padding color (optional, if null the white will be
     *        used)
     */
    public LetterBox(BufferedImage src, int targetW, int targetH, Color padColor) {
        super(targetW, targetH, BufferedImage.TYPE_3BYTE_BGR);

        originalWidth = src.getWidth();
        originalHeight = src.getHeight();

        scale = Math.min((double) targetW / originalWidth, (double) targetH / originalHeight);
        int newW = (int) Math.round(originalWidth * scale);
        int newH = (int) Math.round(originalHeight * scale);

        Graphics2D g = createGraphics();
        try {
            g.setColor(padColor != null ? padColor : Color.WHITE);
            g.fillRect(0, 0, targetW, targetH);
            // high-quality scaling
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            offsetX = (targetW - newW) / 2;
            offsetY = (targetH - newH) / 2;
            g.drawImage(src, offsetX, offsetY, newW, newH, null);
        } finally {
            g.dispose();
        }
    }

    public ImageZone translateToOriginal(ImageZone imageZone) {
        // Translate coordinates back to original image
        double x = Math.max(0, Math.floor((imageZone.getLeft() - offsetX) / scale));
        double y = Math.max(0, Math.floor((imageZone.getTop() - offsetY) / scale));
        double w = Math.min(originalWidth - x, Math.floor(imageZone.getWidth() / scale));
        double h = Math.min(originalHeight - y, Math.floor(imageZone.getHeight() / scale));

        return new ImageZone(x, y, w, h);
    }

    public BufferedImage getImage() {
        return image;
    }

    public void setImage(BufferedImage image) {
        this.image = image;
    }

    @Override
    public String toString() {
        return "Letterbox [scale = " + scale + ", offsetX = " + offsetX + ", offsetY = " + offsetY + "]";
    }
}
