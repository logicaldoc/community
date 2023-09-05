package com.logicaldoc.gui.frontend.client.system.usage;

import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.HTMLPanel;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

/**
 * This dialog shows a usage history chart
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class SystemUsageHistoryChart extends Dialog {

	private static final int DEFAULT_MONTHS = 24;

	private HTMLPanel html = new HTMLPanel("");

	public SystemUsageHistoryChart(String measure, String label) {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("usagehistory") + " - " + I18N.message(label));

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);

		centerInPage();
		setWidth100();
		setHeight100();

		SpinnerItem months = ItemFactory.newSpinnerItem("display", DEFAULT_MONTHS);
		months.setHint(I18N.message("months").toLowerCase());
		months.setMin(6);
		months.setRequired(true);
		months.addChangedHandler(event -> loadChartImage(measure, Integer.parseInt(event.getValue().toString())));

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(months);
		toolStrip.setWidth100();
		addMember(toolStrip);
		addMember(html);

		loadChartImage(measure, DEFAULT_MONTHS);
	}

	private void loadChartImage(String measure, int months) {
		html.setContents(AwesomeFactory.getSpinnerIconHtml("pulse", I18N.message("calculatingstatspleasewait")));

		String chartUrl = Util.contextPath() + "systemusagechart?measure=" + measure + "&locale=" + I18N.getLocale()
				+ "&height=" + (WindowUtils.getHeight() - 100 + "&months=" + months);

		ImageLoader.loadImages(new String[] { chartUrl }, imageElements -> {
			int width = imageElements[0].getWidth();
			int height = imageElements[0].getHeight();
			html.setContents("<div style='overflow:scroll; border: 1px solid green' width='" + width + "px' height='"
					+ height + "px' ><img src='" + chartUrl + "' width='" + width + "' height='" + height
					+ "' /></div>");
			html.setWidth100();
			html.setHeight100();
		});

	}
}