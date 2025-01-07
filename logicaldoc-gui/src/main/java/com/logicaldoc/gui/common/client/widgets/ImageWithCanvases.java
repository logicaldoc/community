package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.MovedEvent;
import com.smartgwt.client.widgets.events.MovedHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;

/**
 * Displays an image and allows the insertion of canvases inside it
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class ImageWithCanvases extends Canvas {

	private Img img;

	private int imageWidth = 1;

	private int imageHeight = 1;

	private List<Canvas> canvases = new ArrayList<>();

	public ImageWithCanvases(String imageUrl, Integer width, List<Canvas> canvases, CallBack loadImageCallback) {
		if (canvases != null)
			this.canvases.addAll(canvases);

		ImageLoader.loadImages(new String[] { imageUrl }, imageElements -> {
			imageWidth = imageElements[0].getWidth();
			imageHeight = imageElements[0].getHeight();

			if (width == null) {
				img.setHeight(getHeight());
				img.setWidth((int) ((double) getHeight() * getImageAspectRatio()));
			} else {
				img.setWidth(width);
				img.setHeight((int) ((double) width / getImageAspectRatio()));
			}

			for (Canvas canvas : ImageWithCanvases.this.canvases) {
				canvas.setOverflow(Overflow.HIDDEN);
				canvas.setSmoothFade(true);
				canvas.addResizedHandler(new CanvasResizedHandler(canvas));
				canvas.addMovedHandler(new CamvasMovedHandler(canvas));
				addChild(canvas);
			}

			if (loadImageCallback != null)
				loadImageCallback.onImagesLoaded(imageElements);
		});

		img = new Img(imageUrl);
		img.setBorder("1px solid gray");

		addChild(img);
	}

	public ImageWithCanvases(String imageUrl, List<Canvas> canvases, CallBack loadImageCallback) {
		this(imageUrl, null, canvases, loadImageCallback);
	}

	/**
	 * Adds a collection of canvases
	 * 
	 * @param canvases the canvases to add
	 */
	public void addCanvases(Collection<Canvas> canvases) {
		for (Canvas canvas : canvases)
			addCanvas(canvas);
	}

	/**
	 * Adds a new canvas
	 * 
	 * @param canvas the canvas to add
	 */
	public void addCanvas(Canvas canvas) {
		canvases.add(canvas);
		canvas.setOverflow(Overflow.HIDDEN);
		canvas.setSmoothFade(true);
		canvas.addResizedHandler(new CanvasResizedHandler(canvas));
		canvas.addMovedHandler(new CamvasMovedHandler(canvas));
		addChild(canvas);
	}

	/**
	 * Removes all the canvases from visualization
	 */
	public void clearCanvases() {
		for (Canvas canvas : canvases)
			removeChild(canvas);
		canvases.clear();
	}

	public int getImageWidth() {
		return imageWidth;
	}

	public int getImageHeight() {
		return imageHeight;
	}

	public double getImageAspectRatio() {
		return (double) imageWidth / (double) imageHeight;
	}

	@Override
	public Canvas setWidth(Integer width) {
		Canvas canvas = super.setWidth(width);
		int newHeight = (int) ((double) width / getImageAspectRatio());
		resize(newHeight);
		return canvas;
	}

	public void resize(int zoom) {
		int newHeight = img.getHeight() + zoom;
		resize2(newHeight);
	}

	private void resize2(int newHeight) {
		if (newHeight <= 0)
			return;

		int newWidth = (int) (newHeight * getImageAspectRatio());

		for (Canvas canvas : canvases) {
			double proportionW = (double) img.getWidth() / (double) canvas.getWidth();
			double proportionH = (double) img.getHeight() / (double) canvas.getHeight();
			double proportionX = (double) img.getWidth() / (double) canvas.getLeft();
			double proportionY = (double) img.getHeight() / (double) canvas.getTop();

			int newCanvasHeight = (int) (newHeight / proportionH);
			int newCanvasWidth = (int) (newWidth / proportionW);
			int newCanvasTop = (int) (newHeight / proportionY);
			int newCanvasLeft = (int) (newWidth / proportionX);

			if (newHeight > 100) {
				canvas.setLeft(newCanvasLeft);
				canvas.setTop(newCanvasTop);
				canvas.setWidth(newCanvasWidth);
				canvas.setHeight(newCanvasHeight);
			}
		}

		if (newHeight > 100) {
			img.setHeight(newHeight);
			img.setWidth(newWidth);
		}
	}

	/**
	 * Converts a left coordinate (0..1) into the actual image left pixel
	 * 
	 * @param lft left coordinate in the range 0..1
	 * 
	 * @return left coordinate in pixels
	 */
	public int computeLeftPixel(double lft) {
		return (int) (lft * img.getWidth().doubleValue());
	}

	/**
	 * Converts a top coordinate (0..1) into the actual image top pixel
	 * 
	 * @param top left coordinate in the range 0..1
	 * @return top coordinate in pixels
	 */
	public int computeTopPixel(double top) {
		return (int) (top * img.getHeight().doubleValue());
	}

	/**
	 * Converts a width dimension (0..1) into the actual image width in pixels
	 * 
	 * @param width width size in the range 0..1
	 * 
	 * @return width size in pixels
	 */
	public int computeWidthPixels(double width) {
		return (int) (width * img.getWidth().doubleValue());
	}

	/**
	 * Converts a height dimension (0..1) into the actual image height in pixels
	 * 
	 * @param height height size in the range 0..1
	 * 
	 * @return height size in pixels
	 */
	public int computeHeightPixels(double height) {
		return (int) (height * img.getHeight().doubleValue());
	}

	/**
	 * Converts a left coordinate in pixels into a virtual left(0..1)
	 * 
	 * @param letfPixel left coordinate expressed in pixels
	 * 
	 * @return the converted left coordinate in range 0..1
	 */
	public double getLeft(int letfPixel) {
		return letfPixel / img.getWidth().doubleValue();
	}

	/**
	 * Converts a top coordinate in pixels into a virtual top(0..1)
	 * 
	 * @param topPixel top coordinate expressed in pixels
	 * 
	 * @return the converted top coordinate in range 0..1
	 */
	public double getTop(int topPixel) {
		return topPixel / img.getHeight().doubleValue();
	}

	/**
	 * Converts a width size in pixels into a virtual width(0..1)
	 * 
	 * @param widthPixel width size expressed in pixels
	 * 
	 * @return the converted width size in range 0..1
	 */
	public double getWidth(int widthPixel) {
		return widthPixel / img.getWidth().doubleValue();
	}

	/**
	 * Converts a height size in pixels into a virtual height(0..1)
	 * 
	 * @param heightPixel height size expressed in pixels
	 * 
	 * @return the converted height size in range 0..1
	 */
	public double getHeight(int heightPixel) {
		return heightPixel / img.getHeight().doubleValue();
	}

	public int getCanvasHeight(Canvas canvas) {
		return (int) ((double) canvas.getHeight() * (double) imageHeight / (double) img.getHeight());
	}

	private class CanvasResizedHandler implements ResizedHandler {
		private Canvas canvas;

		private CanvasResizedHandler(Canvas canvas) {
			this.canvas = canvas;
		}

		@Override
		public void onResized(ResizedEvent event) {
			if (canvas.getLeft() + canvas.getWidth() > img.getWidth())
				canvas.setWidth(img.getWidth() - canvas.getLeft());

			if (canvas.getTop() + canvas.getHeight() > img.getHeight())
				canvas.setHeight(img.getHeight() - canvas.getTop());
		}
	}

	private class CamvasMovedHandler implements MovedHandler {
		private Canvas canvas;

		private CamvasMovedHandler(Canvas canvas) {
			this.canvas = canvas;
		}

		@Override
		public void onMoved(MovedEvent event) {
			if (canvas.getLeft() + canvas.getWidth() > img.getWidth())
				canvas.setLeft(img.getWidth() - canvas.getWidth());
			if (canvas.getTop() + canvas.getHeight() > img.getHeight())
				canvas.setTop(img.getHeight() - canvas.getHeight());

			if (canvas.getLeft() < 0)
				canvas.setLeft(0);
			if (canvas.getTop() < 0)
				canvas.setTop(0);
		}
	}

	public List<Canvas> getCanvases() {
		return canvases;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
}