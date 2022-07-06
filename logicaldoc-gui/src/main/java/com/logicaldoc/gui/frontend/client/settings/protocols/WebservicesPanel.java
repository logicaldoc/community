package com.logicaldoc.gui.frontend.client.settings.protocols;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * The panel showing the webservices settings and the API stats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WebservicesPanel extends VLayout {
	private GUIParameter enabled = null;

	private GUIParameter recCalls = null;

	private GUIParameter callsTtl = null;

	private DynamicForm webServiceForm;

	private GUIParameter[] settings;

	private Img chartImg;

	private StaticTextItem apicalls;

	private StaticTextItem apicallsCurrent;

	public WebservicesPanel(GUIParameter[] settings, ValuesManager vm) {
		this.settings = settings;

		webServiceForm = new DynamicForm();
		webServiceForm.setValuesManager(vm);
		webServiceForm.setTitleOrientation(TitleOrientation.LEFT);
		webServiceForm.setNumCols(4);
		webServiceForm.setWidth(1);
		webServiceForm.setPadding(5);
	}

	@Override
	protected void onDraw() {
		if (!Feature.enabled(Feature.WEBSERVICE)) {
			setMembers(new FeatureDisabled());
			return;
		}

		for (GUIParameter parameter : settings) {
			if (parameter.getName().equals("webservice.enabled"))
				enabled = parameter;
			if (parameter.getName().equals("webservice.call.record"))
				recCalls = parameter;
			if (parameter.getName().equals("webservice.call.ttl"))
				callsTtl = parameter;
		}

		// SOAP Url
		StaticTextItem soapUrl = ItemFactory.newStaticTextItem("soapUrl", I18N.message("soapbaseurl"),
				GWT.getHostPageBaseURL() + "services");
		soapUrl.setColSpan(4);

		// REST Url
		StaticTextItem restUrl = ItemFactory.newStaticTextItem("restUrl", I18N.message("restbaseurl"),
				GWT.getHostPageBaseURL() + "services/rest");
		restUrl.setColSpan(4);

		// Web Service Enabled
		RadioGroupItem wsEnabled = ItemFactory.newBooleanSelector("wsEnabled", "enabled");
		wsEnabled.setRequired(true);
		wsEnabled.setValue(enabled.getValue().equals("true") ? "yes" : "no");
		wsEnabled.setColSpan(4);

		SpinnerItem ttl = ItemFactory.newSpinnerItem("wsTtl", "timetolive", Integer.parseInt(callsTtl.getValue()));
		ttl.setHint(I18N.message("days"));
		ttl.setMin(1);
		ttl.setStep(1);
		ttl.setDisabled(recCalls.getValue().equals("false"));

		// Flag to record webservice calls
		RadioGroupItem recordCalls = ItemFactory.newBooleanSelector("wsRecordCalls", "recordcalls");
		recordCalls.setRequired(true);
		recordCalls.setValue(recCalls.getValue().equals("true") ? "yes" : "no");
		recordCalls.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ttl.setDisabled("no".equals(event.getValue().toString()));
			}
		});

		apicalls = ItemFactory.newStaticTextItem("apicalls", I18N.message("totalcalls"), "0");
		apicallsCurrent = ItemFactory.newStaticTextItem("apicallsCurrent", I18N.message("currentmonthcalls"), "0");
		StaticTextItem apicallsMonthly = ItemFactory.newStaticTextItem("monthlycalls", I18N.message("monthlycalls"),
				"");
		apicallsMonthly.setEndRow(true);

		SelectItem tenantStat = ItemFactory.newTenantSelector();
		tenantStat.setName("tenantStat");
		tenantStat.setTitle(I18N.message("statsfromtenant"));
		tenantStat.setColSpan(4);
		tenantStat.setWidth("*");
		tenantStat.setAllowEmptyValue(true);
		tenantStat.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refreshStats(event.getValue() != null ? Long.parseLong(event.getValue().toString()) : null);
			}
		});

		if (Session.get().isDefaultTenant())
			webServiceForm.setItems(soapUrl, restUrl, wsEnabled, recordCalls, ttl, tenantStat, apicalls,
					apicallsCurrent, apicallsMonthly);
		else
			webServiceForm.setItems(soapUrl, restUrl, apicalls, apicallsCurrent, apicallsMonthly);

		setMembers(webServiceForm);
		if (Session.get().isDefaultTenant())
			refreshStats(null);
		else
			refreshStats(Session.get().getTenantId());
	}

	private void refreshStats(Long tenantId) {
		SettingService.Instance.get().loadWebserviceStats(tenantId, new AsyncCallback<GUIParameter[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIParameter[] parameters) {
				if (parameters != null && parameters.length > 0) {
					for (GUIParameter parameter : parameters) {
						if (parameter.getName().equals("webservice.apicalls"))
							apicalls.setValue(Util.formatLong(Long.parseLong(parameter.getValue())));
						else if (parameter.getName().equals("webservice.apicalls.current"))
							apicallsCurrent.setValue(Util.formatLong(Long.parseLong(parameter.getValue())));
					}
				} else {
					apicalls.setValue("0");
					apicallsCurrent.setValue("0");
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
		String chartUrl = Util.contextPath() + "webservicechart?width=" + chartWidth + "&height=" + chartHeight
				+ "&locale=" + I18N.getLocale() + "&random=" + new Date().getTime();
		if (tenantId != null)
			chartUrl += "&tenantId=" + tenantId;

		chartImg = new Img(chartUrl, 600, 440);
		chartImg.setImageType(ImageStyle.NORMAL);
		// chartImg.setBorder("1px solid gray");
		chartImg.setImageWidth(chartWidth);
		chartImg.setImageHeight(chartHeight);
		chartImg.setOverflow(Overflow.AUTO);
		addMember(chartImg);
	}

	public void save() {
		enabled.setValue(webServiceForm.getValueAsString("wsEnabled").equals("yes") ? "true" : "false");
		recCalls.setValue(webServiceForm.getValueAsString("wsRecordCalls").equals("yes") ? "true" : "false");
		callsTtl.setValue(
				webServiceForm.getValueAsString("wsTtl") != null ? webServiceForm.getValueAsString("wsTtl") : "90");
	}

	public List<GUIParameter> getSettings() {
		List<GUIParameter> settings = new ArrayList<GUIParameter>();
		settings.add(enabled);
		settings.add(recCalls);
		settings.add(callsTtl);
		return settings;
	}
}
