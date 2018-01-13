package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.event.dom.client.LoadEvent;
import com.google.gwt.event.dom.client.LoadHandler;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.RootLayoutPanel;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.EventHandler;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.DragMoveEvent;
import com.smartgwt.client.widgets.events.DragMoveHandler;
import com.smartgwt.client.widgets.events.DragStartEvent;
import com.smartgwt.client.widgets.events.DragStartHandler;

public class DragPanSampleImg extends Img {

	private String imgUrl;

	private boolean loaded = false;

	private int startScrollLeft;

	private int startScrollTop;

	public DragPanSampleImg(String imgUrl) {
		this.imgUrl = imgUrl;
		setWidth(200);
		setHeight(200);
		if (loaded)
			setSrc(imgUrl);
		setOverflow(Overflow.HIDDEN);
		setShowEdges(false);
		setPadding(5);
		setImageType(ImageStyle.NORMAL);
		setCanDrag(true);
		setCursor(Cursor.ALL_SCROLL);
		setDragAppearance(DragAppearance.NONE);
		addDragStartHandler(new DragStartHandler() {
			public void onDragStart(DragStartEvent event) {
				startScrollLeft = getScrollLeft();
				startScrollTop = getScrollTop();
			}
		});
		addDragMoveHandler(new DragMoveHandler() {
			public void onDragMove(DragMoveEvent event) {
				int left = startScrollLeft - EventHandler.getX() + EventHandler.getMouseDownX();
				int top = startScrollTop - EventHandler.getY() + EventHandler.getMouseDownY();
				scrollTo(left, top);
			}
		});

		if (!loaded) {
			final Image image = new Image();
			image.addLoadHandler(new LoadHandler() {
				@Override
				public void onLoad(LoadEvent event) {
					loaded = true;
					if (image.getHeight() > DragPanSampleImg.this.getHeight())
						image.setHeight(DragPanSampleImg.this.getHeight() + "px");
					setSrc(DragPanSampleImg.this.imgUrl);
				}
			});
			image.setVisible(false);
			// Need to attach the Image to the widget tree in order for the load
			// handler to fire.
			RootLayoutPanel.get().add(image);
			image.setUrl(imgUrl);
		}
	}
}