package com.logicaldoc.gui.frontend.client.system.usage;

import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.user.client.ui.RootPanel;
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

	public SystemUsageHistoryChart(String measure, String label, long tenantId) {
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
		months.addChangedHandler(
				event -> loadChartImage(measure, tenantId, Integer.parseInt(event.getValue().toString())));

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(months);
		toolStrip.setWidth100();
		addMember(toolStrip);
		addMember(html);

		loadChartImage(measure, tenantId, DEFAULT_MONTHS);
	}

	private void loadChartImage(String measure, long tenantId, int months) {
		String chartUrl = Util.contextPath() + "systemusagechart?measure=" + measure + "&locale=" + I18N.getLocale()
				+ "&height=" + (WindowUtils.getHeight() - 100 + "&months=" + months + "&tenantId=" + tenantId);
		html.setContents("<div id='chartwaiting'>" + AwesomeFactory.getSpinnerIconHtml("pulse", I18N.message("calculatingstatspleasewait")) + 
		"</div><img src='" + chartUrl + "' onLoad=\"document.getElementById('chartwaiting').setAttribute ('style', 'display: none');\" />");
		html.setWidth100();
		html.setHeight100();

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