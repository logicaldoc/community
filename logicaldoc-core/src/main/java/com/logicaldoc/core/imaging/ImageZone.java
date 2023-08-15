package com.logicaldoc.core.imaging;

/**
 * A zone specification inside an image.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ImageZone {

	/**
	 * Upper-left corner of the zone (percentage of the width 0..1)
	 */
	private Double left = 0.0;

	/**
	 * Upper-left corner of the zone (percentage of the height 0..1)
	 */
	private Double top = 0.0;

	/**
	 * Width of the zone (percentage of the width 0..1)
	 */
	private Double width = 0.15;

	/**
	 * Height of the zone (percentage of the height 0..1)
	 */
	private Double height = 0.10;

	public ImageZone() {
		super();
	}

	public ImageZone(ImageZone source) {
		this.left = source.left;
		this.top = source.top;
		this.width = source.width;
		this.height = source.height;
	}

	public Double getLeft() {
		return left;
	}

	public void setLeft(Double left) {
		this.left = left;
	}

	public Double getTop() {
		return top;
	}

	public void setTop(Double top) {
		this.top = top;
	}

	public Double getWidth() {
		return width;
	}

	public void setWidth(Double width) {
		this.width = width;
	}

	public Double getHeight() {
		return height;
	}

	public void setHeight(Double height) {
		this.height = height;
	}

	public void setArea(double left, double top, double width, double height) {
		this.left = left;
		this.top = top;
		this.width = width;
		this.height = height;
	}
}