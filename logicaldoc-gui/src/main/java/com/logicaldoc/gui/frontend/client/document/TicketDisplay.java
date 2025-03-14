package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.http.client.URL;
import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.DelayedRedrawWindow;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays a created download ticket details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.5
 */
public class TicketDisplay extends DelayedRedrawWindow {

	public TicketDisplay(String ticketId, String sampleUrl1, String sampleUrl2) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("ticket"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		ImageLoader.loadImages(new String[] { Util.qrURL(sampleUrl1,100),Util.qrURL(sampleUrl2,100) }, imageElements -> {
			DynamicForm form = new DynamicForm();
			form.setMargin(4);
			form.setTitleOrientation(TitleOrientation.LEFT);

			StaticTextItem ticketIdItem = ItemFactory.newStaticTextItem("ticketid", ticketId);
			ticketIdItem.setWrap(false);
			ticketIdItem.setWrapTitle(false);

			StaticTextItem sampleUrl1Item = ItemFactory.newStaticTextItem("sampleurl1", I18N.message("sampleurl"),
					"<table border='0'><tr><td>" + Util.qrImg(URL.encode(sampleUrl1), 100) + "</td><td><a href='" + sampleUrl1
							+ "' target='_blank'>" + sampleUrl1 + "</a></td></tr></table>");
			sampleUrl1Item.setWrap(false);
			sampleUrl1Item.setWrapTitle(false);

			StaticTextItem sampleUrl2Item = ItemFactory.newStaticTextItem("sampleurl2", I18N.message("anothersampleurl"),
					"<table border='0'><tr><td>" + Util.qrImg(URL.encode(sampleUrl2), 100) + "</td><td><a href='" + sampleUrl2
					+ "' target='_blank'>" + sampleUrl2 + "</a></td></tr></table>");
			sampleUrl2Item.setWrap(false);
			sampleUrl2Item.setWrapTitle(false);

			StaticTextItem advice = ItemFactory.newStaticTextItem("advice",
					I18N.message("downloadticketdisplay", ticketId));
			advice.setColSpan(2);
			advice.setShowTitle(false);
			advice.setTitleOrientation(TitleOrientation.TOP);

			ButtonItem close = new ButtonItem("close", I18N.message("close"));
			close.addClickHandler(event -> TicketDisplay.this.destroy());

			if (sampleUrl1.equalsIgnoreCase(sampleUrl2))
				form.setItems(advice, new RowSpacerItem(), ticketIdItem, sampleUrl1Item, close);
			else
				form.setItems(advice, new RowSpacerItem(), ticketIdItem, sampleUrl1Item, sampleUrl2Item, close);

			VLayout layout = new VLayout();
			layout.setMembersMargin(5);
			layout.addMember(form);

			addItem(layout);
			
			/*
			 * With a timer we force the redraw
			 */
			delayedRedraw();
		});
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}