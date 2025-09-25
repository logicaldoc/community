package com.logicaldoc.gui.frontend.client.menu.features;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Shows some activable features
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class Features extends Window {

	private static final String DETAILS = "details";

	private static Features instance = new Features();

	private ListGrid grid;

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

		ListGridField name = new ListGridField("name");
		name.setWidth("*");
		name.setCellFormatter((value, rcd, rowNum, colNum) -> I18N.message("feature." + value));

		ListGridField details = new ListGridField(DETAILS);
		details.setAutoFitWidth(true);
		details.setMinWidth(100);
		details.setAlign(Alignment.CENTER);

		ListGridField quote = new ListGridField("quote");
		quote.setAutoFitWidth(true);
		quote.setMinWidth(100);
		quote.setAlign(Alignment.CENTER);

		grid = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord rcd, Integer colNum) {

				String fieldName = this.getFieldName(colNum);

				if (fieldName.equals(DETAILS)) {
					IButton button = new IButton(I18N.message(DETAILS).toLowerCase());
					button.setAutoFit(true);
					button.setMargin(3);
					button.addClickHandler(event -> WindowUtils.openUrl(rcd.getAttributeAsString("url"), "_blank"));
					return button;
				} else if (fieldName.equals("quote")) {
					IButton button = new IButton(I18N.message("requestquote").toLowerCase());
					button.setAutoFit(true);
					button.setMargin(3);
					button.addClickHandler(event -> {
						String featureName = I18N.message("feature." + rcd.getAttribute("name"));
						String body = "Hi, please send me a quote for adding this new optional feature: " + featureName
								+ ".%0D%0A";
						body += "%0D%0AFeaure code: " + rcd.getAttribute("name");
						body += "%0D%0AUserNo: " + Session.get().getInfo().getUserNo();
						body += "%0D%0AProduct: " + Session.get().getInfo().getBranding().getProductName();
						body += "%0D%0ALicensee: " + Session.get().getInfo().getLicensee();
						body += "%0D%0A%0D%0A";
						Util.uninstallCloseWindowAlert();
						WindowUtils.openUrl("mailto:" + Session.get().getInfo().getBranding().getSales() + "?subject="
								+ "Quote request, feature " + featureName + "&body=" + body);
						Util.installCloseWindowAlert();
					});
					return button;
				} else {
					return null;
				}
			}
		};

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
		grid.setShowRecordComponents(true);
		grid.setShowRecordComponentsByCell(true);

		grid.setFields(name, details, quote);
		grid.setCanResizeFields(true);
		grid.setDataSource(new FeaturesDS());

		Label summary = new Label(I18N.message("activablefeaturessummary"));
		summary.setAutoHeight();
		summary.setMinHeight(50);

		addItem(summary);
		addItem(grid);
		grid.draw();
	}

	public static Features get() {
		return instance;
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