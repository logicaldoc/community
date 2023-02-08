package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays a created download ticket details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.5
 */
public class DownloadTicketDisplay extends Window {

	public DownloadTicketDisplay(String ticketId, String sampleUrl1, String sampleUrl2) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("downloadticket"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		DynamicForm form = new DynamicForm();
		form.setMargin(4);
		form.setTitleOrientation(TitleOrientation.LEFT);

		StaticTextItem ticketIdItem = ItemFactory.newStaticTextItem("ticketid", ticketId);
		ticketIdItem.setWrap(false);
		ticketIdItem.setWrapTitle(false);

		StaticTextItem sampleUrl1Item = ItemFactory.newStaticTextItem("sampleurl1", 
				"<a href='" + sampleUrl1 + "' target='_blank'>" + sampleUrl1 + "</a>");
		sampleUrl1Item.setWrap(false);
		sampleUrl1Item.setWrapTitle(false);

		StaticTextItem sampleUrl2Item = ItemFactory.newStaticTextItem("sampleurl2", I18N.message("anothersampleurl"),
				"<a href='" + sampleUrl2 + "' target='_blank'>" + sampleUrl2 + "</a>");
		sampleUrl2Item.setWrap(false);
		sampleUrl2Item.setWrapTitle(false);

		StaticTextItem advice = ItemFactory.newStaticTextItem("advice", 
				I18N.message("downloadticketdisplay", ticketId));
		advice.setColSpan(2);
		advice.setShowTitle(false);
		advice.setTitleOrientation(TitleOrientation.TOP);

		ButtonItem close = new ButtonItem("close", I18N.message("close"));
		close.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				DownloadTicketDisplay.this.destroy();
			}
		});

		if (sampleUrl1.equalsIgnoreCase(sampleUrl2))
			form.setItems(advice, new RowSpacerItem(), ticketIdItem, sampleUrl1Item, close);
		else
			form.setItems(advice, new RowSpacerItem(), ticketIdItem, sampleUrl1Item, sampleUrl2Item, close);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.addMember(form);

		addItem(layout);
	}
}