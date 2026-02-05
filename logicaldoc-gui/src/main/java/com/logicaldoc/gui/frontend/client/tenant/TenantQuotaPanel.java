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

	private static final String TICKETSQUOTA = "ticketsquota";

	private static final String WORKFLOWSQUOTA = "workflowsquota";

	private static final String FORMSQUOTA = "formsquota";

	private static final String REPORTSQUOTA = "reportsquota";

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
		usersQuota.setMin(1);
		usersQuota.setStep(1);
		usersQuota.setWidth(80);
		usersQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			usersQuota.addChangedHandler(changedHandler);

		SpinnerItem guestsQuota = ItemFactory.newSpinnerItem(GUESTSQUOTA, "readonlyusersquota", tenant.getMaxGuests());
		guestsQuota.setDisabled(readonly);
		guestsQuota.setRequired(false);
		guestsQuota.setMin(-1);
		guestsQuota.setStep(1);
		guestsQuota.setWidth(80);
		guestsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			guestsQuota.addChangedHandler(changedHandler);

		SpinnerItem sessionsQuota = ItemFactory.newSpinnerItem(SESSIONSQUOTA, tenant.getMaxSessions());
		sessionsQuota.setDisabled(readonly);
		sessionsQuota.setRequired(false);
		sessionsQuota.setMin(-1);
		sessionsQuota.setStep(1);
		sessionsQuota.setWidth(80);
		sessionsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			sessionsQuota.addChangedHandler(changedHandler);

		SpinnerItem monthlyApiCallsQuota = ItemFactory.newSpinnerItem(MONTHLYAPICALLSQUOTA, tenant.getMaxApiCalls());
		monthlyApiCallsQuota.setDisabled(readonly);
		monthlyApiCallsQuota.setRequired(false);
		monthlyApiCallsQuota.setMin(-1);
		monthlyApiCallsQuota.setStep(10);
		monthlyApiCallsQuota.setWidth(100);
		monthlyApiCallsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			monthlyApiCallsQuota.addChangedHandler(changedHandler);

		SpinnerItem documentsQuota = ItemFactory.newSpinnerItem(DOCUMENTSQUOTA, tenant.getMaxRepoDocs());
		documentsQuota.setDisabled(readonly);
		documentsQuota.setRequired(false);
		documentsQuota.setMin(-1);
		documentsQuota.setStep(10000);
		documentsQuota.setWidth(100);
		documentsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			documentsQuota.addChangedHandler(changedHandler);

		SpinnerItem sizeQuota = ItemFactory.newSpinnerItem(SIZEQUOTA, tenant.getMaxRepoSize());
		sizeQuota.setHint("MB");
		sizeQuota.setDisabled(readonly);
		sizeQuota.setRequired(false);
		sizeQuota.setMin(-1);
		sizeQuota.setStep(1024);
		sizeQuota.setWidth(100);
		sizeQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			sizeQuota.addChangedHandler(changedHandler);

		SpinnerItem ticketsQuota = ItemFactory.newSpinnerItem(TICKETSQUOTA, tenant.getMaxTickets());
		ticketsQuota.setDisabled(readonly);
		ticketsQuota.setRequired(false);
		ticketsQuota.setMin(-1);
		ticketsQuota.setStep(10);
		ticketsQuota.setWidth(100);
		ticketsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			ticketsQuota.addChangedHandler(changedHandler);

		SpinnerItem workflowsQuota = ItemFactory.newSpinnerItem(WORKFLOWSQUOTA, tenant.getMaxWorkflows());
		workflowsQuota.setDisabled(readonly);
		workflowsQuota.setRequired(false);
		workflowsQuota.setMin(-1);
		workflowsQuota.setStep(1);
		workflowsQuota.setWidth(100);
		workflowsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			workflowsQuota.addChangedHandler(changedHandler);

		SpinnerItem formsQuota = ItemFactory.newSpinnerItem(FORMSQUOTA, tenant.getMaxForms());
		formsQuota.setDisabled(readonly);
		formsQuota.setRequired(false);
		formsQuota.setMin(-1);
		formsQuota.setStep(1);
		formsQuota.setWidth(100);
		formsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			formsQuota.addChangedHandler(changedHandler);

		SpinnerItem reportsQuota = ItemFactory.newSpinnerItem(REPORTSQUOTA, tenant.getMaxReports());
		reportsQuota.setDisabled(readonly);
		reportsQuota.setRequired(false);
		reportsQuota.setMin(-1);
		reportsQuota.setStep(1);
		reportsQuota.setWidth(100);
		reportsQuota.setVisible(!tenant.isSystem());
		if (!readonly)
			reportsQuota.addChangedHandler(changedHandler);

		SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
				tenant.getQuotaThreshold());
		quotaThreshold.setDisabled(readonly);
		quotaThreshold.setMax(100);
		quotaThreshold.setMin(-1);
		quotaThreshold.setHint("%");
		quotaThreshold.setVisible(!tenant.isSystem());
		if (!readonly)
			quotaThreshold.addChangedHandler(changedHandler);

		recipients = ItemFactory.newMultiComboBoxItem("recipients", "alertrecipients", new UsersDS(null, false, false),
				tenant.getQuotaAlertRecipients().toArray(new String[0]));
		recipients.setDisabled(readonly);
		recipients.setValueField("username");
		recipients.setDisplayField("username");
		recipients.setVisible(!tenant.isSystem());
		if (!readonly)
			recipients.addChangedHandler(changedHandler);

		StaticTextItem size = ItemFactory.newStaticTextItem("ssize", "size", Util.formatSizeW7(tenant.getSize()));
		size.setWrap(false);
		StaticTextItem documents = ItemFactory.newStaticTextItem("documents", Util.formatLong(tenant.getDocuments()));
		StaticTextItem sessions = ItemFactory.newStaticTextItem("sessions", Util.formatLong(tenant.getSessions()));
		StaticTextItem apicalls = ItemFactory.newStaticTextItem("monthlyapicalls",
				Util.formatLong(tenant.getApiCalls()));
		StaticTextItem users = ItemFactory.newStaticTextItem("users", Util.formatLong(tenant.getUsers()));
		StaticTextItem guests = ItemFactory.newStaticTextItem("guests", "readonlyusers",
				Util.formatLong(tenant.getGuests()));
		StaticTextItem tickets = ItemFactory.newStaticTextItem("tickets", Util.formatLong(tenant.getTickets()));
		StaticTextItem workflows = ItemFactory.newStaticTextItem("workflows", Util.formatLong(tenant.getWorkflows()));
		StaticTextItem forms = ItemFactory.newStaticTextItem("forms", Util.formatLong(tenant.getForms()));
		StaticTextItem reports = ItemFactory.newStaticTextItem("reports", Util.formatLong(tenant.getReports()));

		// Static items to display whole system quotas
		StaticTextItem usersSystemQuota = ItemFactory.newStaticTextItem("sys" + USERSQUOTA, USERSQUOTA,
				Util.formatInt(tenant.getMaxUsers()));
		usersSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem guestsSystemQuota = ItemFactory.newStaticTextItem("sys" + GUESTSQUOTA, "readonlyusersquota",
				Util.formatInt(tenant.getMaxGuests()));
		guestsSystemQuota.setVisible(tenant.isSystem());

		StaticTextItem sessionsSystemQuota = ItemFactory.newStaticTextItem("sys" + SESSIONSQUOTA, SESSIONSQUOTA,
				Util.formatInt(tenant.getMaxSessions()));
		sessionsSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem documentsSystemQuota = ItemFactory.newStaticTextItem("sys" + DOCUMENTSQUOTA, DOCUMENTSQUOTA,
				Util.formatLong(tenant.getMaxRepoDocs()));
		documentsSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem sizeSystemQuota = ItemFactory.newStaticTextItem("sys" + SIZEQUOTA, SIZEQUOTA,
				Util.formatSizeW7(tenant.getMaxRepoSize() * 1024L * 1024L));
		sizeSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem monthlyApiCallsSystemQuota = ItemFactory.newStaticTextItem("sys" + MONTHLYAPICALLSQUOTA,
				MONTHLYAPICALLSQUOTA, Util.formatLong(tenant.getMaxApiCalls()));
		monthlyApiCallsSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem ticketsSystemQuota = ItemFactory.newStaticTextItem("sys" + TICKETSQUOTA, TICKETSQUOTA,
				Util.formatLong(tenant.getMaxTickets()));
		ticketsSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem workflowsSystemQuota = ItemFactory.newStaticTextItem("sys" + WORKFLOWSQUOTA, WORKFLOWSQUOTA,
				Util.formatLong(tenant.getMaxWorkflows()));
		workflowsSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem formsSystemQuota = ItemFactory.newStaticTextItem("sys" + FORMSQUOTA, FORMSQUOTA,
				Util.formatLong(tenant.getMaxForms()));
		formsSystemQuota.setVisible(tenant.isSystem());
		StaticTextItem reportsSystemQuota = ItemFactory.newStaticTextItem("sys" + REPORTSQUOTA, REPORTSQUOTA,
				Util.formatLong(tenant.getMaxReports()));
		reportsSystemQuota.setVisible(tenant.isSystem());

		form.setItems(usersQuota, usersSystemQuota, users, guestsQuota, guestsSystemQuota, guests, sessionsQuota,
				sessionsSystemQuota, sessions, documentsQuota, documentsSystemQuota, documents, sizeQuota,
				sizeSystemQuota, size, monthlyApiCallsQuota, monthlyApiCallsSystemQuota, apicalls, ticketsQuota,
				ticketsSystemQuota, tickets, workflowsQuota, workflowsSystemQuota, workflows, formsSystemQuota,
				formsQuota, forms, reportsSystemQuota, reportsQuota, reports, quotaThreshold, recipients);
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

		if (values.get(TICKETSQUOTA) == null)
			tenant.setMaxTickets(null);
		else
			tenant.setMaxTickets(Long.parseLong(values.get(TICKETSQUOTA).toString()));

		if (values.get(WORKFLOWSQUOTA) == null)
			tenant.setMaxWorkflows(null);
		else
			tenant.setMaxWorkflows(Long.parseLong(values.get(WORKFLOWSQUOTA).toString()));

		if (values.get(FORMSQUOTA) == null)
			tenant.setMaxForms(null);
		else
			tenant.setMaxForms(Long.parseLong(values.get(FORMSQUOTA).toString()));

		if (values.get(REPORTSQUOTA) == null)
			tenant.setMaxReports(null);
		else
			tenant.setMaxReports(Long.parseLong(values.get(REPORTSQUOTA).toString()));

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