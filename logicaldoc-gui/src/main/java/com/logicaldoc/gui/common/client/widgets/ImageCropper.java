package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.MovedEvent;
import com.smartgwt.client.widgets.events.MovedHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;

/**
 * Displays an image and allows the seletion of an area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.1
 */
public class ImageCropper extends Canvas {

	private Img img;

	private int imageWidth = 1;

	private int imageHeight = 1;

	private Canvas selection = new Canvas();

	private boolean maintainSelectionAspectRatio = true;

	private double selectionAspectRatio = 1.3;

	public ImageCropper(String imageUrl, int initialSelectionWidth, double selectionAspectRatio) {
		setSelectionAspectRatio(selectionAspectRatio);

		ImageLoader.loadImages(new String[] { imageUrl },  imageElements -> {
				imageWidth = imageElements[0].getWidth();
				imageHeight = imageElements[0].getHeight();

				img.setHeight(getHeight());
				img.setWidth((int) ((double) getHeight() * getImageAspectRatio()));
		});

		img = new Img(imageUrl);
		img.setBorder("1px solid gray");

		selection.setOverflow(Overflow.HIDDEN);
		selection.setBorder("1px solid yellow");
		selection.setBackgroundColor("gray");
		selection.setCanDragReposition(true);
		selection.setCanDragResize(true);
		selection.setDragAppearance(DragAppearance.TARGET);
		selection.setSmoothFade(true);
		selection.setLeft(100);
		selection.setTop(100);
		selection.setWidth(initialSelectionWidth);
		selection.setHeight(
				selectionAspectRatio > 0 ? (int) ((double) initialSelectionWidth / selectionAspectRatio) : 40);
		selection.setOpacity(60);
		selection.addResizedHandler( event -> {
				if (maintainSelectionAspectRatio)
					selection.setHeight((int) ((double) selection.getWidth() / selectionAspectRatio));

				if (selection.getLeft() + selection.getWidth() > img.getWidth())
					selection.setWidth(img.getWidth() - selection.getLeft());

				if (selection.getTop() + selection.getHeight() > img.getHeight())
					selection.setHeight(img.getHeight() - selection.getTop());
		});

		selection.addMovedHandler(event -> {
				if (selection.getLeft() + selection.getWidth() > img.getWidth())
					selection.setLeft(img.getWidth() - selection.getWidth());
				if (selection.getTop() + selection.getHeight() > img.getHeight())
					selection.setTop(img.getHeight() - selection.getHeight());

				if (selection.getLeft() < 0)
					selection.setLeft(0);
				if (selection.getTop() < 0)
					selection.setTop(0);
		});

		addChild(img);
		addChild(selection);
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

	public boolean isMaintainSelectionAspectRatio() {
		return maintainSelectionAspectRatio;
	}

	public double getSelectionAspectRatio() {
		return selectionAspectRatio;
	}

	public void setMaintainSelectionAspectRatio(boolean maintainSelectionAspectRatio) {
		this.maintainSelectionAspectRatio = maintainSelectionAspectRatio;
	}

	public void setSelectionAspectRatio(double selectionAspectRatio) {
		this.selectionAspectRatio = selectionAspectRatio;
		if (selectionAspectRatio <= 0)
			setMaintainSelectionAspectRatio(false);
	}

	public void resize(int zoom) {
		int newHeight = img.getHeight() + zoom;
		int newWidth = (int) ((double) newHeight * getImageAspectRatio());

		double proportionW = img.getWidth() / (double) selection.getWidth();
		double proportionH = img.getHeight() / (double) selection.getHeight();
		double proportionX = img.getWidth() / (double) selection.getLeft();
		double proportionY = img.getHeight() / (double) selection.getTop();

		int newSelectionHeight = (int) ((double) newHeight / proportionH);
		int newSelectionWidth = (int) ((double) newWidth / proportionW);
		int newSelectionTop = (int) ((double) newHeight / proportionY);
		int newSelectionLeft = (int) ((double) newWidth / proportionX);

		if (img != null && newHeight > 100) {
			img.setHeight(newHeight);
			img.setWidth(newWidth);

			selection.setLeft(newSelectionLeft);
			selection.setTop(newSelectionTop);
			selection.setWidth(newSelectionWidth);
			selection.setHeight(newSelectionHeight);
		}
	}

	public int getSelectionXCoordinate() {
		return (int) ((double) selection.getLeft() * (double) imageWidth / (double) img.getWidth());
	}

	public int getSelectionYCoordinate() {
		return (int) ((double) selection.getTop() * (double) imageHeight / (double) img.getHeight());
	}

	public int getSelectionWidth() {
		return (int) ((double) selection.getWidth() * (double) imageWidth / (double) img.getWidth());
	}

	public int getSelectionHeight() {
		return (int) ((double) selection.getHeight() * (double) imageHeight / (double) img.getHeight());
	}
}