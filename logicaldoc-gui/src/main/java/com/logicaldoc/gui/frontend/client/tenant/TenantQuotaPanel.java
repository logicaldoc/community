package com.logicaldoc.gui.frontend.client.tenant;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TenantQuotaPanel extends HLayout {
	private static final String MONTHLYAPICALLSQUOTA = "monthlyapicallsquota";

	private static final String QUOTA_THRESHOLD = "quotaThreshold";

	private static final String SIZEQUOTA = "sizequota";

	private static final String DOCUMENTSQUOTA = "documentsquota";

	private static final String SESSIONSQUOTA = "sessionsquota";

	private static final String GUESTSQUOTA = "guestsquota";

	private static final String USERSQUOTA = "usersquota";

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUITenant tenant;

	private ChangedHandler changedHandler;

	private VLayout layout = new VLayout();

	private MultiComboBoxItem recipients;

	public TenantQuotaPanel(GUITenant tenant, ChangedHandler changedHandler) {
		if (tenant == null) {
			setMembers(TenantsPanel.SELECT_TENANT);
		} else {
			this.tenant = tenant;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);
			layout.setWidth(300);
			refresh();
		}
	}

	public void refresh() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(true);
		vm = new ValuesManager();

		if (form != null)
			form.destroy();

		if (Boolean.TRUE.equals(contains(form)))
			removeChild(form);

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		layout.addMember(form, 1);

		SpinnerItem usersQuota = ItemFactory.newSpinnerItem(USERSQUOTA, tenant.getMaxUsers());
		usersQuota.setDisabled(readonly);
		usersQuota.setRequired(false);
		usersQuota.setMin(tenant.getUsers());
		usersQuota.setStep(1);
		usersQuota.setWidth(80);
		if (!readonly)
			usersQuota.addChangedHandler(changedHandler);

		SpinnerItem guestsQuota = ItemFactory.newSpinnerItem(GUESTSQUOTA, "readonlyusersquota", tenant.getMaxGuests());
		guestsQuota.setDisabled(readonly);
		guestsQuota.setRequired(false);
		guestsQuota.setMin(tenant.getGuests());
		guestsQuota.setStep(1);
		guestsQuota.setWidth(80);
		if (!readonly)
			guestsQuota.addChangedHandler(changedHandler);

		SpinnerItem sessionsQuota = ItemFactory.newSpinnerItem(SESSIONSQUOTA, tenant.getMaxSessions());
		sessionsQuota.setDisabled(readonly);
		sessionsQuota.setRequired(false);
		sessionsQuota.setMin(0);
		sessionsQuota.setStep(1);
		sessionsQuota.setWidth(80);
		if (!readonly)
			sessionsQuota.addChangedHandler(changedHandler);

		SpinnerItem monthlyApiCallsQuota = ItemFactory.newSpinnerItem(MONTHLYAPICALLSQUOTA, tenant.getMaxApiCalls());
		monthlyApiCallsQuota.setDisabled(readonly);
		monthlyApiCallsQuota.setRequired(false);
		monthlyApiCallsQuota.setMin(0);
		monthlyApiCallsQuota.setStep(10);
		monthlyApiCallsQuota.setWidth(100);
		if (!readonly)
			monthlyApiCallsQuota.addChangedHandler(changedHandler);
		
		SpinnerItem documentsQuota = ItemFactory.newSpinnerItem(DOCUMENTSQUOTA, tenant.getMaxRepoDocs());
		documentsQuota.setDisabled(readonly);
		documentsQuota.setRequired(false);
		documentsQuota.setMin(0);
		documentsQuota.setStep(10000);
		documentsQuota.setWidth(100);
		if (!readonly)
			documentsQuota.addChangedHandler(changedHandler);

		SpinnerItem sizeQuota = ItemFactory.newSpinnerItem(SIZEQUOTA, tenant.getMaxRepoSize());
		sizeQuota.setHint("MB");
		sizeQuota.setDisabled(readonly);
		sizeQuota.setRequired(false);
		sizeQuota.setMin(0);
		sizeQuota.setStep(1024);
		sizeQuota.setWidth(100);
		if (!readonly)
			sizeQuota.addChangedHandler(changedHandler);

		SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
				tenant.getQuotaThreshold());
		quotaThreshold.setDisabled(readonly);
		quotaThreshold.setMax(100);
		quotaThreshold.setMin(0);
		quotaThreshold.setHint("%");
		if (!readonly)
			quotaThreshold.addChangedHandler(changedHandler);

		recipients = ItemFactory.newMultiComboBoxItem("recipients", "alertrecipients", new UsersDS(null, false, false),
				tenant.getQuotaAlertRecipients().toArray(new String[0]));
		recipients.setDisabled(readonly);
		recipients.setValueField("username");
		recipients.setDisplayField("username");
		if (!readonly)
			recipients.addChangedHandler(changedHandler);

		StaticTextItem size = ItemFactory.newStaticTextItem("ssize", "size", Util.formatSizeW7(tenant.getSize()));
		size.setWrap(false);

		StaticTextItem documents = ItemFactory.newStaticTextItem("documents", Util.formatLong(tenant.getDocuments()));
		StaticTextItem sessions = ItemFactory.newStaticTextItem("sessions", Util.formatLong(tenant.getSessions()));
		StaticTextItem apicalls = ItemFactory.newStaticTextItem("monthlyapicalls", Util.formatLong(tenant.getApiCalls()));
		StaticTextItem users = ItemFactory.newStaticTextItem("users", Util.formatLong(tenant.getUsers()));
		StaticTextItem guests = ItemFactory.newStaticTextItem("guests", "readonlyusers",
				Util.formatLong(tenant.getGuests()));

		form.setItems(usersQuota, users, guestsQuota, guests, sessionsQuota, sessions, documentsQuota, documents,
				sizeQuota, size, monthlyApiCallsQuota, apicalls, quotaThreshold, recipients);
		addMember(layout);
	}

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = vm.getValues();
		if (Boolean.FALSE.equals(vm.validate()))
			return false;

		if (values.get(DOCUMENTSQUOTA) == null)
			tenant.setMaxRepoDocs(null);
		else
			tenant.setMaxRepoDocs(Long.parseLong(values.get(DOCUMENTSQUOTA).toString()));

		if (values.get(SIZEQUOTA) == null)
			tenant.setMaxRepoSize(null);
		else
			tenant.setMaxRepoSize(Long.parseLong(values.get(SIZEQUOTA).toString()));

		if (values.get(MONTHLYAPICALLSQUOTA) == null)
			tenant.setMaxApiCalls(null);
		else
			tenant.setMaxApiCalls(Long.parseLong(values.get(MONTHLYAPICALLSQUOTA).toString()));
		
		if (values.get(USERSQUOTA) == null)
			tenant.setMaxUsers(null);
		else
			tenant.setMaxUsers(Integer.parseInt(values.get(USERSQUOTA).toString()));

		if (values.get(GUESTSQUOTA) == null)
			tenant.setMaxGuests(null);
		else
			tenant.setMaxGuests(Integer.parseInt(values.get(GUESTSQUOTA).toString()));

		if (values.get(SESSIONSQUOTA) == null)
			tenant.setMaxSessions(null);
		else
			tenant.setMaxSessions(Integer.parseInt(values.get(SESSIONSQUOTA).toString()));

		if (values.get(QUOTA_THRESHOLD) == null)
			tenant.setQuotaThreshold(null);
		else
			tenant.setQuotaThreshold(Integer.parseInt(values.get(QUOTA_THRESHOLD).toString()));		
		
		setQuotaAlertRecipients();

		return !vm.hasErrors();
	}

	private void setQuotaAlertRecipients() {
		tenant.getQuotaAlertRecipients().clear();
		String[] usernames = recipients.getValues();
		if (usernames != null && usernames.length > 0)
			for (int i = 0; i < usernames.length; i++)
				tenant.addQuotaAlertRecipient(usernames[i]);
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