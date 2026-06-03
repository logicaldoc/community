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

    private BufferedImage src;

    private double scale = 1;

    private int offsetX;

    private int offsetY;

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

        this.src = src;

        int srcWidth = src.getWidth();
        int srcHeight = src.getHeight();

        scale = Math.min((double) targetW / srcWidth, (double) targetH / srcHeight);
        int newW = (int) Math.round(srcWidth * scale);
        int newH = (int) Math.round(srcHeight * scale);

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

    /**
     * Crop a sub-image of the source image corresponding to a zone related to
     * the letterbox.
     * 
     * @param imageZone The zone to crop expressed in coordinates related to
     *        letterbox
     * 
     * @return The zone with image taken from the source image
     */
    public ImageZone cropFromFromSource(ImageZone imageZone) {
        // Translate coordinates back to original image
        double x = Math.max(0, Math.floor((imageZone.getLeft() - offsetX) / scale));
        double y = Math.max(0, Math.floor((imageZone.getTop() - offsetY) / scale));
        double w = Math.min(src.getWidth() - x, Math.floor(imageZone.getWidth() / scale));
        double h = Math.min(src.getHeight() - y, Math.floor(imageZone.getHeight() / scale));

        ImageZone label = new ImageZone(x, y, w, h);
        label.setImage(ImageUtil.crop(src, label));

        return label;
    }

    @Override
    public String toString() {
        return "Letterbox [scale = " + scale + ", offsetX = " + offsetX + ", offsetY = " + offsetY + "]";
    }
}
