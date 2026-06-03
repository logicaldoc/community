package com.logicaldoc.core.imaging;

import java.awt.image.BufferedImage;
import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;

/**
 * A zone specification inside an image.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
@MappedSuperclass
public class ImageZone implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Upper-left corner of the zone (may be a coordinate or a percentage of the
     * width 0..1)
     */
    @Column(name = "ld_left")
    protected double left = 0.0;

    /**
     * Upper-left corner of the zone (may be a coordinate or a percentage of the
     * height 0..1)
     */
    @Column(name = "ld_top")
    protected double top = 0.0;

    /**
     * Width of the zone (may be a coordinate or a percentage of the width 0..1)
     */
    @Column(name = "ld_width")
    protected double width = 0.15;

    /**
     * Height of the zone (may be a coordinate or a percentage of the height
     * 0..1)
     */
    @Column(name = "ld_height")
    protected double height = 0.10;

    protected transient BufferedImage image;

    public ImageZone() {
        super();
    }

    public ImageZone(double left, double top, double width, double height) {
        super();
        this.left = left;
        this.top = top;
        this.width = width;
        this.height = height;
    }

    public ImageZone(ImageZone source) {
        this.left = source.left;
        this.top = source.top;
        this.width = source.width;
        this.height = source.height;
        this.image = source.image;
    }

    public BufferedImage getImage() {
        return image;
    }

    public void setImage(BufferedImage image) {
        this.image = image;
    }

    public Point getCorner1() {
        return new Point(left, top);
    }

    public Point getCorner2() {
        return new Point(left + width, top + height);
    }

    public double getLeft() {
        return left;
    }

    public void setLeft(double left) {
        this.left = left;
    }

    public double getTop() {
        return top;
    }

    public void setTop(double top) {
        this.top = top;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    public void setArea(double left, double top, double width, double height) {
        this.left = left;
        this.top = top;
        this.width = width;
        this.height = height;
    }

    public record Point(double x, double y) {
        public double distance(Point other) {
            double w = Math.abs(this.x() - other.x());
            double h = Math.abs(this.y() - other.y());

            return Math.sqrt(Math.pow(w, 2) + Math.pow(h, 2));
        }
    }

    @Override
    public String toString() {
        return "ImageZone [left=" + left + ", top=" + top + ", width=" + width + ", height=" + height + "]";
    }
}