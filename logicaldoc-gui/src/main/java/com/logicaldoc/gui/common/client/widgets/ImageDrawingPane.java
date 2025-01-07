package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.drawing.DrawImage;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.drawing.DrawPane;

/**
 * Displays an image with drawings over it
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class ImageDrawingPane extends DrawPane {

	private int imageWidth = 1;

	private int imageHeight = 1;

	private String imageUrl;

	private DrawImage background = new DrawImage();

	public ImageDrawingPane(String imageUrl, CallBack loadImageCallback) {
		this.imageUrl = imageUrl;

		setShowEdges(true);
		setEdgeSize(1);
		setCursor(Cursor.AUTO);
		setOverflow(Overflow.AUTO);
		setBackgroundColor("papayawhip");

		ImageLoader.loadImages(new String[] { imageUrl }, imageElements -> {
			imageWidth = imageElements[0].getWidth();
			imageHeight = imageElements[0].getHeight();

			setWidth(imageWidth);
			setHeight(imageHeight);

			for (DrawItem item : getDrawItems())
				item.setDrawPane(ImageDrawingPane.this);

			initDrawings();

			if (loadImageCallback != null)
				loadImageCallback.onImagesLoaded(imageElements);
		});
	}

	public double getImageAspectRatio() {
		return (double) imageWidth / (double) imageHeight;
	}

	private void initDrawings() {
		erase();

		background = new DrawImage();
		background.setLeft(0);
		background.setTop(0);
		background.setWidth(imageWidth);
		background.setHeight(imageHeight);
		background.setSrc(imageUrl);
		background.setLineColor("#808080");
		background.setLineWidth(2);
		background.setDrawPane(this);
		background.draw();

		background.addClickHandler(event -> {
			for (DrawItem item : getDrawItems()) {
				item.hideAllKnobs();
				item.setCanDrag(false);
			}
		});

		/*
		 * Draw the annotations once the background has been drawn
		 */
		background.addDrawEndHandler(event -> {
			for (DrawItem item : getDrawItems()) {
				item.setDrawPane(ImageDrawingPane.this);
				item.draw();
			}
		});

		draw();
	}

	public int getImageWidth() {
		return imageWidth;
	}

	public int getImageHeight() {
		return imageHeight;
	}

	/**
	 * Gets all the items but not the background
	 * 
	 * @return the list of items
	 */
	public List<DrawItem> getItems() {
		List<DrawItem> itms = new ArrayList<>();
		for (DrawItem item : getDrawItems()) {
			if (!item.equals(background))
				itms.add(item);
		}
		return itms;
	}

	@Override
	public void zoom(double zoomLevel) {
		super.zoom(zoomLevel);
		setWidth((int) (imageWidth * zoomLevel));
		setHeight((int) (imageHeight * zoomLevel));
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
}