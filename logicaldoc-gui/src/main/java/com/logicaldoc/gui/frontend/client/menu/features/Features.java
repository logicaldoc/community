package com.logicaldoc.gui.frontend.client.menu.features;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * Shows some activable features
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class Features extends Window {

	private static Features instance = new Features();

	private ListGrid grid = new ListGrid();

	public Features() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("activablefeatures"));
		setWidth(400);
		setHeight(300);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		grid.setWidth100();
		grid.setHeight100();
		grid.setCanReorderFields(false);
		grid.setCanFreezeFields(false);
		grid.setCanGroupBy(false);
		grid.setCanSort(false);
		grid.setAutoFetchData(true);
		grid.setSelectionType(SelectionStyle.NONE);
		grid.setFilterOnKeypress(false);
		grid.setShowFilterEditor(false);
		grid.setShowHeader(false);

		ListGridField name = new ListGridField("name");
		name.setWidth("*");
		name.setCellFormatter((value, rcd, rowNum, colNum) -> I18N.message("feature." + value));

		ListGridField url = new ListGridField("url");
		url.setCellFormatter((value, rcd, rowNum, colNum) -> "<a href='" + value + "' target='_blank'>"
				+ I18N.message("moredetails").toLowerCase() + "</a>");
		url.setAutoFitWidth(true);

		ListGridField quote = new ListGridField("mailto");
		quote.setAutoFitWidth(true);
		quote.setCellFormatter((value, rcd, rowNum, colNum) -> {
			String featureName = I18N.message("feature." + rcd.getAttribute("name"));
			String body = "Feature: " + featureName;
			body += "%0D%0AFeaure code: " + rcd.getAttribute("name");
			body += "%0D%0AUserNo: " + Session.get().getInfo().getUserNo();
			body += "%0D%0AProduct: " + Session.get().getInfo().getBranding().getProductName();
			body += "%0D%0A%0D%0A";
			return "<a href='mailto:" + Session.get().getInfo().getBranding().getSales() + "?subject="
					+ I18N.message("pleasequotefeature") + " " + featureName + "&body=" + body + "'>"
					+ I18N.message("requestquote").toLowerCase() + "</a>";
		});

		grid.setFields(name, url, quote);
		grid.setCanResizeFields(true);
		grid.setDataSource(new FeaturesDS());

		Label summary = new Label(I18N.message("activablefeaturessummary"));
		summary.setAutoHeight();
		summary.setMinHeight(50);
		
		addItem(summary);
		addItem(grid);
	}

	public static Features get() {
		return instance;
	}
}