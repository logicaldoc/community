package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows AI's statistics.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class AIStatsPanel extends VLayout {

	private StaticTextItem aiQueries;

	private StaticTextItem aiQueriesCurrent;

	private Img chartImg;

	private Long modelId = null;

	public AIStatsPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(4);
	}

	public AIStatsPanel(long modelId) {
		this();
		this.modelId = modelId;
	}

	@Override
	protected void onDraw() {
		aiQueries = ItemFactory.newStaticTextItem("aiQueries", I18N.message("totalqueries"), "0");
		aiQueriesCurrent = ItemFactory.newStaticTextItem("aiQueriesCurrent", I18N.message("currentmonthqueries"), "0");

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		form.setWidth(1);
		form.setPadding(5);

		SelectItem tenantStat = prepareTenantStatItem();

		form.setItems(tenantStat, aiQueries, aiQueriesCurrent);

		addMember(form);

		refreshStats(null);
	}

	private SelectItem prepareTenantStatItem() {
		SelectItem tenantStat = ItemFactory.newTenantSelector();
		tenantStat.setName("tenantStat");
		tenantStat.setTitle(I18N.message("statsfromtenant"));
		tenantStat.setColSpan(4);
		tenantStat.setWidth("*");
		tenantStat.setAllowEmptyValue(true);
		tenantStat.setVisible(modelId == null);
		tenantStat.addChangedHandler(tenantStatChanged -> refreshStats(
				tenantStatChanged.getValue() != null ? Long.parseLong(tenantStatChanged.getValue().toString()) : null));
		return tenantStat;
	}

	private void refreshStats(Long tenantId) {
		AIService.Instance.get().getStats(modelId, tenantId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIParameter> parameters) {
				if (!parameters.isEmpty()) {
					for (GUIParameter parameter : parameters) {
						if (parameter.getName().equals("ai.queries"))
							aiQueries.setValue(Util.formatLong(Long.parseLong(parameter.getValue())));
						else if (parameter.getName().equals("ai.queries.current"))
							aiQueriesCurrent.setValue(Util.formatLong(Long.parseLong(parameter.getValue())));
					}
				} else {
					aiQueries.setValue("0");
					aiQueriesCurrent.setValue("0");
				}

				refreshStatsChart(tenantId);
			}
		});
	}

	private void refreshStatsChart(Long tenantId) {
		if (chartImg != null)
			removeMember(chartImg);

		int chartWidth = 1200;
		int chartHeight = 420;
		String chartUrl = Util.contextPath() + "aichart?width=" + chartWidth + "&height=" + chartHeight + "&locale="
				+ I18N.getLocale() + "&random=" + new Date().getTime();
		if (modelId != null)
			chartUrl += "&modelId=" + modelId;
		if (tenantId != null)
			chartUrl += "&tenantId=" + tenantId;

		chartImg = new Img(chartUrl, 600, 440);
		chartImg.setImageType(ImageStyle.NORMAL);
		chartImg.setImageWidth(chartWidth);
		chartImg.setImageHeight(chartHeight);
		chartImg.setOverflow(Overflow.AUTO);

		addMember(chartImg);
	}

	boolean validate() {
		return true;
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