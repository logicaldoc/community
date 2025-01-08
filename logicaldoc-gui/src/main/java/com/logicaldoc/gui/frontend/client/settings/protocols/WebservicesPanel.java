package com.logicaldoc.gui.frontend.client.settings.protocols;

import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
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
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * The panel showing the webservices settings and the API stats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WebservicesPanel extends VLayout {

	private static final String WEBSERVICE_CALL_TTL = "webservice.call.ttl";

	private static final String WEBSERVICE_CALL_RECORD_PAYLOAD = "webservice.call.record.payload";

	private static final String WEBSERVICE_CALL_RECORD = "webservice.call.record";

	private static final String WEBSERVICE_BASICAUTH_ENABLED = "webservice.basicauth.enabled";

	private static final String WEBSERVICE_ENABLED = "webservice.enabled";

	private static final String FALSE = "false";

	private static final String WS_TTL = "wsTtl";

	private List<GUIParameter> settings;

	private DynamicForm webServiceForm;

	private Img chartImg;

	private StaticTextItem apicalls;

	private StaticTextItem apicallsCurrent;

	public WebservicesPanel(List<GUIParameter> settings, ValuesManager vm) {
		webServiceForm = new DynamicForm();
		webServiceForm.setValuesManager(vm);
		webServiceForm.setTitleOrientation(TitleOrientation.LEFT);
		webServiceForm.setNumCols(4);
		webServiceForm.setWidth(1);
		webServiceForm.setPadding(5);

		this.settings = settings;
	}

	@Override
	protected void onDraw() {
		if (!Feature.enabled(Feature.WEBSERVICE)) {
			setMembers(new FeatureDisabled());
			return;
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
		ToggleItem wsEnabled = prepareEnabledItem();

		// Web Service Basic Authentication Enabled
		ToggleItem wsBasicAuthEnabled = prepareBasicAuthEnabledItem();

		// Flag to rec webservice calls payload
		ToggleItem recordCallsPayload = prepareRecordCallsPayloadItem();

		SpinnerItem ttl = ItemFactory.newSpinnerItem(WS_TTL, "timetolive",
				Integer.parseInt(Util.getParameterValue(settings, WEBSERVICE_CALL_TTL)));
		ttl.setHint(I18N.message("days"));
		ttl.setMin(1);
		ttl.setStep(1);
		ttl.setDisabled(FALSE.equals(Util.getParameterValue(settings, WEBSERVICE_CALL_TTL)));

		// Flag to record webservice calls
		ToggleItem recordCalls = ItemFactory.newToggleItem("wsRecordCalls", "recordcalls",
				Util.getParameterValueAsBoolean(settings, WEBSERVICE_CALL_RECORD));
		recordCalls.setRequired(true);
		recordCalls.addChangedHandler(event -> {
			ttl.setDisabled("no".equals(event.getValue().toString()));
			recordCallsPayload.setDisabled("no".equals(event.getValue().toString()));
		});

		apicalls = ItemFactory.newStaticTextItem("apicalls", I18N.message("totalcalls"), "0");
		apicallsCurrent = ItemFactory.newStaticTextItem("apicallsCurrent", I18N.message("currentmonthcalls"), "0");

		SelectItem tenantStat = prepareTenantStatItem();

		if (Session.get().isDefaultTenant())
			webServiceForm.setItems(soapUrl, restUrl, wsEnabled, wsBasicAuthEnabled, recordCalls, recordCallsPayload,
					ttl, tenantStat, apicalls, apicallsCurrent);
		else
			webServiceForm.setItems(soapUrl, restUrl, wsEnabled, wsBasicAuthEnabled, apicalls, apicallsCurrent);

		setMembers(webServiceForm);
		if (Session.get().isDefaultTenant())
			refreshStats(null);
		else
			refreshStats(Session.get().getTenantId());
	}

	private ToggleItem prepareRecordCallsPayloadItem() {
		ToggleItem recordCallsPayload = ItemFactory.newToggleItem("wsRecordCallsPayload", "recordcallspayload",
				Util.getParameterValueAsBoolean(settings, WEBSERVICE_CALL_RECORD_PAYLOAD));
		recordCallsPayload.setWrapTitle(false);
		recordCallsPayload.setRequired(true);
		return recordCallsPayload;
	}

	private SelectItem prepareTenantStatItem() {
		SelectItem tenantStat = ItemFactory.newTenantSelector();
		tenantStat.setName("tenantStat");
		tenantStat.setTitle(I18N.message("statsfromtenant"));
		tenantStat.setColSpan(4);
		tenantStat.setWidth("*");
		tenantStat.setAllowEmptyValue(true);
		tenantStat.addChangedHandler(tenantStatChanged -> refreshStats(
				tenantStatChanged.getValue() != null ? Long.parseLong(tenantStatChanged.getValue().toString()) : null));
		return tenantStat;
	}

	private ToggleItem prepareEnabledItem() {
		ToggleItem wsEnabled = ItemFactory.newToggleItem("wsEnabled", "enabled",
				Util.getParameterValueAsBoolean(settings, WEBSERVICE_ENABLED));
		wsEnabled.setRequired(true);
		wsEnabled.setColSpan(4);
		wsEnabled.setDisabled(!Session.get().isDefaultTenant());
		return wsEnabled;
	}

	private ToggleItem prepareBasicAuthEnabledItem() {
		ToggleItem toggleItem = ItemFactory.newToggleItem("wsBasicAuthEnabled", "enablebasicauth",
				Util.getParameterValueAsBoolean(settings, WEBSERVICE_BASICAUTH_ENABLED));
		toggleItem.setColSpan(4);
		toggleItem.setWrapTitle(false);
		toggleItem.setDisabled(!Session.get().isDefaultTenant());
		return toggleItem;
	}

	private void refreshStats(Long tenantId) {
		SettingService.Instance.get().loadWebserviceStats(tenantId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIParameter> parameters) {
				if (!parameters.isEmpty()) {
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
		chartImg.setImageWidth(chartWidth);
		chartImg.setImageHeight(chartHeight);
		chartImg.setOverflow(Overflow.AUTO);
		addMember(chartImg);
	}

	public void save() {
		Util.getParameter(settings, WEBSERVICE_ENABLED).setValue(webServiceForm.getValueAsString("wsEnabled"));
		Util.getParameter(settings, WEBSERVICE_BASICAUTH_ENABLED)
				.setValue(webServiceForm.getValueAsString("wsBasicAuthEnabled"));
		Util.getParameter(settings, WEBSERVICE_CALL_RECORD).setValue(webServiceForm.getValueAsString("wsRecordCalls"));
		Util.getParameter(settings, WEBSERVICE_CALL_RECORD_PAYLOAD)
				.setValue(webServiceForm.getValueAsString("wsRecordCallsPayload"));
		Util.getParameter(settings, WEBSERVICE_CALL_TTL).setValue(
				webServiceForm.getValueAsString(WS_TTL) != null ? webServiceForm.getValueAsString(WS_TTL) : "90");
	}

	public List<GUIParameter> getSettings() {
		return settings;
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