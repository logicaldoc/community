package com.logicaldoc.gui.frontend.client.tenant;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
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

    private static final String THRESHOLD = "threshold";

    private static final String ALERTREACHED = "alertreached";

    private static final String ALERTFREQUENCY = "alertfrequency";

    private static final String PAGES_QUOTA = "pagesquota";

    private static final String MONTHLYAPICALLS_QUOTA = "monthlyapicallsquota";

    private static final String QUOTA_THRESHOLD = "quotaThreshold";

    private static final String STORAGE_QUOTA = "storagequota";

    private static final String DOCUMENTS_QUOTA = "documentsquota";

    private static final String SESSIONS_QUOTA = "sessionsquota";

    private static final String READONLY_USERS_QUOTA = "readonlyusersquota";

    private static final String REGULAR_USERS_QUOTA = "regularusersquota";

    private static final String TICKETS_QUOTA = "ticketsquota";

    private static final String WORKFLOWS_QUOTA = "workflowsquota";

    private static final String FORMS_QUOTA = "formsquota";

    private static final String REPORTS_QUOTA = "reportsquota";

    private static final String STAMPS_QUOTA = "stampsquota";

    private static final String IMPORTFOLDERS_QUOTA = "importfoldersquota";

    private static final String EMAILACCOUNTS_QUOTA = "emailaccountsquota";

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

        SpinnerItem[] regularUsersQuota = prepareRegularUsersQuotaItems();

        SpinnerItem[] readonlyUsersQuota = prepareReadonlyUsersQuotaItems();

        SpinnerItem[] sessionsQuota = prepareSessionsQuotaItems();

        SpinnerItem[] documentsQuota = prepareDocumentsQuotaItems();

        SpinnerItem[] pagesQuota = preparePagesQuotaItems();

        SpinnerItem[] storageQuota = prepareStorageQuotaItems();

        SpinnerItem[] monthlyApiCallsQuota = prepareMonthlyApiCallsQuotaItems();

        SpinnerItem[] ticketsQuota = prepareTicketsQuoteItems();

        SpinnerItem[] workflowsQuota = prepareWorkflowQuotaItems();

        SpinnerItem[] formsQuota = prepareFormsQuotaItems();

        SpinnerItem[] reportsQuota = prepareReportsQuotaItems();

        SpinnerItem[] stampsQuota = prepareStampsQuotaItems();

        SpinnerItem[] importFoldersQuota = prepareImportFoldersQuotaItem();

        SpinnerItem[] emailAccountsQuota = prepareEmailAccountsQuotaItems();

        SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
                tenant.getQuotaThreshold());
        quotaThreshold.setStartRow(true);
        quotaThreshold.setDisabled(changedHandler == null);
        quotaThreshold.setMax(100);
        quotaThreshold.setMin(-1);
        quotaThreshold.setHint("%");
        quotaThreshold.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            quotaThreshold.addChangedHandler(changedHandler);

        SpinnerItem frequency = ItemFactory.newSpinnerItem(ALERTFREQUENCY, ALERTFREQUENCY,
                tenant.getQuotaAlertFrequency());
        frequency.setRequired(true);
        frequency.setDisabled(changedHandler == null);
        frequency.setMin(1);
        frequency.setHint(I18N.message("hours").toLowerCase());
        frequency.setVisible(tenant.isDefault());
        if (changedHandler != null)
            frequency.addChangedHandler(changedHandler);

        CheckboxItem reached = ItemFactory.newCheckbox(ALERTREACHED, "alertwhenreached");
        reached.setValue(tenant.isQuotaAlertReached());
        reached.setDisabled(changedHandler == null);
        reached.setVisible(tenant.isDefault());
        if (changedHandler != null)
            reached.addChangedHandler(changedHandler);

        recipients = ItemFactory.newMultiComboBoxItem("recipients", "alertrecipients", new UsersDS(null, false, false),
                tenant.getQuotaAlertRecipients().toArray(new String[0]));
        recipients.setDisabled(changedHandler == null);
        recipients.setValueField("username");
        recipients.setDisplayField("username");
        recipients.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            recipients.addChangedHandler(changedHandler);

        // Static items to display whole system quotas
        StaticTextItem regularUsersSystemQuota = ItemFactory.newStaticTextItem("sys" + REGULAR_USERS_QUOTA,
                REGULAR_USERS_QUOTA, Util.formatLong(tenant.getRegularUsersQuota()));
        regularUsersSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem readonlyUsersSystemQuota = ItemFactory.newStaticTextItem("sys" + READONLY_USERS_QUOTA,
                READONLY_USERS_QUOTA, Util.formatLong(tenant.getReadonlyUsersQuota()));
        readonlyUsersSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem sessionsSystemQuota = ItemFactory.newStaticTextItem("sys" + SESSIONS_QUOTA, SESSIONS_QUOTA,
                Util.formatLong(tenant.getSessionsQuota()));
        sessionsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem documentsSystemQuota = ItemFactory.newStaticTextItem("sys" + DOCUMENTS_QUOTA, DOCUMENTS_QUOTA,
                Util.formatLong(tenant.getDocumentsQuota()));
        documentsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem pagesSystemQuota = ItemFactory.newStaticTextItem("sys" + PAGES_QUOTA, PAGES_QUOTA,
                Util.formatLong(tenant.getPagesQuota()));
        pagesSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem storageSystemQuota = ItemFactory.newStaticTextItem("sys" + STORAGE_QUOTA, STORAGE_QUOTA,
                tenant.getStorageQuota() != null ? Util.formatSizeW7((double) tenant.getStorageQuota() * 1024D * 1024D)
                        : null);
        storageSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem monthlyApiCallsSystemQuota = ItemFactory.newStaticTextItem("sys" + MONTHLYAPICALLS_QUOTA,
                MONTHLYAPICALLS_QUOTA, Util.formatLong(tenant.getApiCallsQuota()));
        monthlyApiCallsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem ticketsSystemQuota = ItemFactory.newStaticTextItem("sys" + TICKETS_QUOTA, TICKETS_QUOTA,
                Util.formatLong(tenant.getTicketsQuota()));
        ticketsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem workflowsSystemQuota = ItemFactory.newStaticTextItem("sys" + WORKFLOWS_QUOTA, WORKFLOWS_QUOTA,
                Util.formatLong(tenant.getWorkflowsQuota()));
        workflowsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem formsSystemQuota = ItemFactory.newStaticTextItem("sys" + FORMS_QUOTA, FORMS_QUOTA,
                Util.formatLong(tenant.getFormsQuota()));
        formsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem reportsSystemQuota = ItemFactory.newStaticTextItem("sys" + REPORTS_QUOTA, REPORTS_QUOTA,
                Util.formatLong(tenant.getReportsQuota()));
        reportsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem stampsSystemQuota = ItemFactory.newStaticTextItem("sys" + STAMPS_QUOTA, STAMPS_QUOTA,
                Util.formatLong(tenant.getStampsQuota()));
        stampsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem importFoldersSystemQuota = ItemFactory.newStaticTextItem("sys" + IMPORTFOLDERS_QUOTA,
                IMPORTFOLDERS_QUOTA, Util.formatLong(tenant.getImportFoldersQuota()));
        importFoldersSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem emailAccountsSystemQuota = ItemFactory.newStaticTextItem("sys" + EMAILACCOUNTS_QUOTA,
                EMAILACCOUNTS_QUOTA, Util.formatLong(tenant.getEmailAccountsQuota()));
        emailAccountsSystemQuota.setVisible(tenant.isSystem());

        form.setItems(regularUsersQuota[0], regularUsersQuota[1], regularUsersSystemQuota, readonlyUsersQuota[0],
                readonlyUsersQuota[1], readonlyUsersSystemQuota, sessionsQuota[0], sessionsQuota[1],
                sessionsSystemQuota, documentsQuota[0], documentsQuota[1], documentsSystemQuota, pagesQuota[0],
                pagesQuota[1], pagesSystemQuota, storageQuota[0], storageQuota[1], storageSystemQuota,
                monthlyApiCallsQuota[0], monthlyApiCallsQuota[1], monthlyApiCallsSystemQuota, ticketsQuota[0],
                ticketsQuota[1], ticketsSystemQuota, workflowsQuota[0], workflowsQuota[1], workflowsSystemQuota,
                formsSystemQuota, formsQuota[0], formsQuota[1], reportsSystemQuota, reportsQuota[0], reportsQuota[1],
                stampsSystemQuota, stampsQuota[0], stampsQuota[1], importFoldersSystemQuota, importFoldersQuota[0],
                importFoldersQuota[1], emailAccountsSystemQuota, emailAccountsQuota[0], emailAccountsQuota[1],
                quotaThreshold, frequency, reached, recipients);
        addMember(layout);
    }

    private SpinnerItem[] prepareEmailAccountsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(EMAILACCOUNTS_QUOTA, tenant.getEmailAccountsQuota(),
                tenant.getEmailAccounts());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(EMAILACCOUNTS_QUOTA + THRESHOLD,
                tenant.getEmailAccountsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareImportFoldersQuotaItem() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(IMPORTFOLDERS_QUOTA, tenant.getImportFoldersQuota(),
                tenant.getImportFolders());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(IMPORTFOLDERS_QUOTA + THRESHOLD,
                tenant.getImportFoldersQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareStampsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(STAMPS_QUOTA, tenant.getStampsQuota(), tenant.getStamps());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(STAMPS_QUOTA + THRESHOLD,
                tenant.getStampsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareReportsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(REPORTS_QUOTA, tenant.getReportsQuota(),
                tenant.getReports());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(REPORTS_QUOTA + THRESHOLD,
                tenant.getReportsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareFormsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(FORMS_QUOTA, tenant.getFormsQuota(), tenant.getForms());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(FORMS_QUOTA + THRESHOLD,
                tenant.getFormsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareWorkflowQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(WORKFLOWS_QUOTA, tenant.getWorkflowsQuota(),
                tenant.getWorkflows());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(WORKFLOWS_QUOTA + THRESHOLD,
                tenant.getWorkflowsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareTicketsQuoteItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(TICKETS_QUOTA, tenant.getTicketsQuota(),
                tenant.getTickets());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(TICKETS_QUOTA + THRESHOLD,
                tenant.getTicketsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareStorageQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(STORAGE_QUOTA, tenant.getStorageQuota(),
                tenant.getStorage());
        quota.setHint("MB " + I18N.message("usedhint", Util.formatSizeW7(tenant.getStorage())));
        quota.setStep(1024);

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(STORAGE_QUOTA + THRESHOLD,
                tenant.getStorageQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareDocumentsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(DOCUMENTS_QUOTA, tenant.getDocumentsQuota(),
                tenant.getDocuments());
        quota.setStep(1000);

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(DOCUMENTS_QUOTA + THRESHOLD,
                tenant.getDocumentsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] preparePagesQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(PAGES_QUOTA, tenant.getPagesQuota(), tenant.getPages());
        quota.setStep(1000);

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(PAGES_QUOTA + THRESHOLD,
                tenant.getPagesQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareMonthlyApiCallsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(MONTHLYAPICALLS_QUOTA, tenant.getApiCallsQuota(),
                tenant.getApiCalls());
        quota.setStep(1000);

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(MONTHLYAPICALLS_QUOTA + THRESHOLD,
                tenant.getApiCallsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareSessionsQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(SESSIONS_QUOTA, tenant.getSessionsQuota(),
                tenant.getSessions());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(SESSIONS_QUOTA + THRESHOLD,
                tenant.getSessionsQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareReadonlyUsersQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(READONLY_USERS_QUOTA, tenant.getReadonlyUsersQuota(),
                tenant.getReadonlyUsers());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(READONLY_USERS_QUOTA + THRESHOLD,
                tenant.getReadonlyUsersQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem[] prepareRegularUsersQuotaItems() {
        SpinnerItem quota = ItemFactory.newQuotaSpinnerItem(REGULAR_USERS_QUOTA, tenant.getRegularUsersQuota(),
                tenant.getRegularUsers());

        SpinnerItem threshold = ItemFactory.newQuotaAlertThresholdSpinnerItem(REGULAR_USERS_QUOTA + THRESHOLD,
                tenant.getRegularUsersQuotaThreshold());

        return new SpinnerItem[] { applyQuotaItemVisibility(quota), applyQuotaItemVisibility(threshold) };
    }

    private SpinnerItem applyQuotaItemVisibility(SpinnerItem item) {
        item.setDisabled(tenant.isSystem());
        if (changedHandler != null)
            item.addChangedHandler(changedHandler);
        return item;
    }

    @SuppressWarnings("unchecked")
    public boolean validate() {
        Map<String, Object> values = vm.getValues();
        if (Boolean.FALSE.equals(vm.validate()))
            return false;

        validateDocumentsQuota(values);

        validatePagesQuota(values);

        validateStorageQuota(values);

        validateApiCallsQuota(values);

        validateUsersQuota(values);

        validateSessionsQuota(values);

        validateTicketsQuota(values);

        validateWorkflowsQuota(values);

        validateFormsQuota(values);

        validateReportsQuota(values);

        validateStampsQuota(values);

        validateImportFoldersQuota(values);

        validateEmailAccountsQuota(values);

        if (values.get(QUOTA_THRESHOLD) == null)
            tenant.setQuotaThreshold(null);
        else
            tenant.setQuotaThreshold(Integer.parseInt(values.get(QUOTA_THRESHOLD).toString()));

        setQuotaAlertRecipients();

        if (tenant.isDefault()) {
            tenant.setQuotaAlertFrequency(Integer.parseInt(values.get(ALERTFREQUENCY).toString()));
            tenant.setQuotaAlertReached(Boolean.parseBoolean(values.get(ALERTREACHED).toString()));
        }

        return !vm.hasErrors();
    }

    private void validateEmailAccountsQuota(Map<String, Object> values) {
        if (values.get(EMAILACCOUNTS_QUOTA) == null)
            tenant.setEmailAccountsQuota(null);
        else
            tenant.setEmailAccountsQuota(Long.parseLong(values.get(EMAILACCOUNTS_QUOTA).toString()));

        if (values.get(EMAILACCOUNTS_QUOTA + THRESHOLD) == null)
            tenant.setEmailAccountsQuotaThreshold(null);
        else
            tenant.setEmailAccountsQuotaThreshold(
                    Integer.parseInt(values.get(EMAILACCOUNTS_QUOTA + THRESHOLD).toString()));
    }

    private void validateImportFoldersQuota(Map<String, Object> values) {
        if (values.get(IMPORTFOLDERS_QUOTA) == null)
            tenant.setImportFoldersQuota(null);
        else
            tenant.setImportFoldersQuota(Long.parseLong(values.get(IMPORTFOLDERS_QUOTA).toString()));

        if (values.get(IMPORTFOLDERS_QUOTA + THRESHOLD) == null)
            tenant.setImportFoldersQuotaThreshold(null);
        else
            tenant.setImportFoldersQuotaThreshold(
                    Integer.parseInt(values.get(IMPORTFOLDERS_QUOTA + THRESHOLD).toString()));
    }

    private void validateStampsQuota(Map<String, Object> values) {
        if (values.get(STAMPS_QUOTA) == null)
            tenant.setStampsQuota(null);
        else
            tenant.setStampsQuota(Long.parseLong(values.get(STAMPS_QUOTA).toString()));

        if (values.get(STAMPS_QUOTA + THRESHOLD) == null)
            tenant.setStampsQuotaThreshold(null);
        else
            tenant.setStampsQuotaThreshold(Integer.parseInt(values.get(STAMPS_QUOTA + THRESHOLD).toString()));
    }

    private void validateReportsQuota(Map<String, Object> values) {
        if (values.get(REPORTS_QUOTA) == null)
            tenant.setReportsQuota(null);
        else
            tenant.setReportsQuota(Long.parseLong(values.get(REPORTS_QUOTA).toString()));

        if (values.get(REPORTS_QUOTA + THRESHOLD) == null)
            tenant.setReportsQuotaThreshold(null);
        else
            tenant.setReportsQuotaThreshold(Integer.parseInt(values.get(REPORTS_QUOTA + THRESHOLD).toString()));
    }

    private void validateFormsQuota(Map<String, Object> values) {
        if (values.get(FORMS_QUOTA) == null)
            tenant.setFormsQuota(null);
        else
            tenant.setFormsQuota(Long.parseLong(values.get(FORMS_QUOTA).toString()));

        if (values.get(FORMS_QUOTA + THRESHOLD) == null)
            tenant.setFormsQuotaThreshold(null);
        else
            tenant.setFormsQuotaThreshold(Integer.parseInt(values.get(FORMS_QUOTA + THRESHOLD).toString()));
    }

    private void validateWorkflowsQuota(Map<String, Object> values) {
        if (values.get(WORKFLOWS_QUOTA) == null)
            tenant.setWorkflowsQuota(null);
        else
            tenant.setWorkflowsQuota(Long.parseLong(values.get(WORKFLOWS_QUOTA).toString()));

        if (values.get(WORKFLOWS_QUOTA + THRESHOLD) == null)
            tenant.setWorkflowsQuotaThreshold(null);
        else
            tenant.setWorkflowsQuotaThreshold(Integer.parseInt(values.get(WORKFLOWS_QUOTA + THRESHOLD).toString()));
    }

    private void validateTicketsQuota(Map<String, Object> values) {
        if (values.get(TICKETS_QUOTA) == null)
            tenant.setTicketsQuota(null);
        else
            tenant.setTicketsQuota(Long.parseLong(values.get(TICKETS_QUOTA).toString()));

        if (values.get(TICKETS_QUOTA + THRESHOLD) == null)
            tenant.setTicketsQuotaThreshold(null);
        else
            tenant.setTicketsQuotaThreshold(Integer.parseInt(values.get(TICKETS_QUOTA + THRESHOLD).toString()));
    }

    private void validateSessionsQuota(Map<String, Object> values) {
        if (values.get(SESSIONS_QUOTA) == null)
            tenant.setSessionsQuota(null);
        else
            tenant.setSessionsQuota(Long.parseLong(values.get(SESSIONS_QUOTA).toString()));

        if (values.get(SESSIONS_QUOTA + THRESHOLD) == null)
            tenant.setSessionsQuotaThreshold(null);
        else
            tenant.setSessionsQuotaThreshold(Integer.parseInt(values.get(SESSIONS_QUOTA + THRESHOLD).toString()));
    }

    private void validateUsersQuota(Map<String, Object> values) {
        if (values.get(REGULAR_USERS_QUOTA) == null)
            tenant.setRegularUsersQuota(null);
        else
            tenant.setRegularUsersQuota(Long.parseLong(values.get(REGULAR_USERS_QUOTA).toString()));

        if (values.get(REGULAR_USERS_QUOTA + THRESHOLD) == null)
            tenant.setRegularUsersQuotaThreshold(null);
        else
            tenant.setRegularUsersQuotaThreshold(
                    Integer.parseInt(values.get(REGULAR_USERS_QUOTA + THRESHOLD).toString()));
    }

    private void validateApiCallsQuota(Map<String, Object> values) {
        if (values.get(MONTHLYAPICALLS_QUOTA) == null)
            tenant.setApiCallsQuota(null);
        else
            tenant.setApiCallsQuota(Long.parseLong(values.get(MONTHLYAPICALLS_QUOTA).toString()));

        if (values.get(MONTHLYAPICALLS_QUOTA + THRESHOLD) == null)
            tenant.setApiCallsQuotaThreshold(null);
        else
            tenant.setApiCallsQuotaThreshold(
                    Integer.parseInt(values.get(MONTHLYAPICALLS_QUOTA + THRESHOLD).toString()));
    }

    private void validateStorageQuota(Map<String, Object> values) {
        if (values.get(STORAGE_QUOTA) == null)
            tenant.setStorageQuota(null);
        else
            tenant.setStorageQuota(Long.parseLong(values.get(STORAGE_QUOTA).toString()));

        if (values.get(STORAGE_QUOTA + THRESHOLD) == null)
            tenant.setStorageQuotaThreshold(null);
        else
            tenant.setStorageQuotaThreshold(Integer.parseInt(values.get(STORAGE_QUOTA + THRESHOLD).toString()));
    }

    private void validateDocumentsQuota(Map<String, Object> values) {
        if (values.get(DOCUMENTS_QUOTA) == null)
            tenant.setDocumentsQuota(null);
        else
            tenant.setDocumentsQuota(Long.parseLong(values.get(DOCUMENTS_QUOTA).toString()));

        if (values.get(DOCUMENTS_QUOTA + THRESHOLD) == null)
            tenant.setDocumentsQuotaThreshold(null);
        else
            tenant.setDocumentsQuotaThreshold(Integer.parseInt(values.get(DOCUMENTS_QUOTA + THRESHOLD).toString()));
    }

    private void validatePagesQuota(Map<String, Object> values) {
        if (values.get(PAGES_QUOTA) == null)
            tenant.setPagesQuota(null);
        else
            tenant.setPagesQuota(Long.parseLong(values.get(PAGES_QUOTA).toString()));

        if (values.get(PAGES_QUOTA + THRESHOLD) == null)
            tenant.setPagesQuotaThreshold(null);
        else
            tenant.setPagesQuotaThreshold(Integer.parseInt(values.get(PAGES_QUOTA + THRESHOLD).toString()));
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