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

        SpinnerItem regularUsersQuota = prepareRegularUsersQuotaItem();

        SpinnerItem readonlyUsersQuota = prepareReadonlyUsersQuotaItem();

        SpinnerItem sessionsQuota = prepareSessionsQuotaItem();

        SpinnerItem monthlyApiCallsQuota = prepareMonthlyApiCallsQuotaItem();

        SpinnerItem documentsQuota = prepareDocumentsQuotaItem();

        SpinnerItem pagesQuota = preparePagesQuotaItem();

        SpinnerItem sizeQuota = prepareStorageQuotaItem();

        SpinnerItem ticketsQuota = prepareTicketsQuoteItem();

        SpinnerItem workflowsQuota = prepareWorkflowQuotaItem();

        SpinnerItem formsQuota = prepareFormsQuotaItem();

        SpinnerItem reportsQuota = prepareReportsQuotaItem();

        SpinnerItem stampsQuota = prepareStampsQuotaItem();

        SpinnerItem importFoldersQuota = prepareImportFoldersQuotaItem();

        SpinnerItem emailAccountsQuota = prepareEmailAccountsQuotaItem();

        SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
                tenant.getQuotaThreshold());
        quotaThreshold.setDisabled(changedHandler == null);
        quotaThreshold.setMax(100);
        quotaThreshold.setMin(-1);
        quotaThreshold.setHint("%");
        quotaThreshold.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            quotaThreshold.addChangedHandler(changedHandler);

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
                "readonlyusersquota", Util.formatLong(tenant.getReadonlyUsersQuota()));
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
        StaticTextItem sizeSystemQuota = ItemFactory.newStaticTextItem("sys" + STORAGE_QUOTA, STORAGE_QUOTA,
                tenant.getStorageQuota() != null ? Util.formatSizeW7((double) tenant.getStorageQuota() * 1024D * 1024D)
                        : null);
        sizeSystemQuota.setVisible(tenant.isSystem());
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

        form.setItems(regularUsersQuota, regularUsersSystemQuota, readonlyUsersQuota, readonlyUsersSystemQuota,
                sessionsQuota, sessionsSystemQuota, documentsQuota, documentsSystemQuota, pagesQuota, pagesSystemQuota,
                sizeQuota, sizeSystemQuota, monthlyApiCallsQuota, monthlyApiCallsSystemQuota, ticketsQuota,
                ticketsSystemQuota, workflowsQuota, workflowsSystemQuota, formsSystemQuota, formsQuota,
                reportsSystemQuota, reportsQuota, stampsSystemQuota, stampsQuota, importFoldersSystemQuota,
                importFoldersQuota, emailAccountsSystemQuota, emailAccountsQuota, quotaThreshold, recipients);
        addMember(layout);
    }

    private SpinnerItem prepareEmailAccountsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(EMAILACCOUNTS_QUOTA, tenant.getEmailAccountsQuota(),
                tenant.getEmailAccounts());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareImportFoldersQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(IMPORTFOLDERS_QUOTA, tenant.getImportFoldersQuota(),
                tenant.getImportFolders());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareStampsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(STAMPS_QUOTA, tenant.getStampsQuota(),
                tenant.getStamps());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareReportsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(REPORTS_QUOTA, tenant.getReportsQuota(),
                tenant.getReports());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareFormsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(FORMS_QUOTA, tenant.getFormsQuota(),
                tenant.getForms());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareWorkflowQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(WORKFLOWS_QUOTA, tenant.getWorkflowsQuota(),
                tenant.getWorkflows());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareTicketsQuoteItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(TICKETS_QUOTA, tenant.getTicketsQuota(),
                tenant.getTickets());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareStorageQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(STORAGE_QUOTA, tenant.getStorageQuota(),
                tenant.getStorage());    
        item.setHint("MB " + I18N.message("usedhint", Util.formatSizeW7(tenant.getStorage())));
        item.setStep(1024);
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareDocumentsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(DOCUMENTS_QUOTA, tenant.getDocumentsQuota(),
                tenant.getDocuments());
        item.setStep(1000);
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem preparePagesQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(PAGES_QUOTA, tenant.getPagesQuota(), tenant.getPages());
        item.setStep(1000);
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareMonthlyApiCallsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(MONTHLYAPICALLS_QUOTA, tenant.getApiCallsQuota(),
                tenant.getApiCalls());
        item.setStep(1000);
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareSessionsQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(SESSIONS_QUOTA, tenant.getSessionsQuota(),
                tenant.getSessions());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareReadonlyUsersQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(READONLY_USERS_QUOTA, tenant.getReadonlyUsersQuota(),
                tenant.getReadonlyUsers());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem prepareRegularUsersQuotaItem() {
        SpinnerItem item = ItemFactory.newQuotaSpinnerItem(REGULAR_USERS_QUOTA, tenant.getRegularUsersQuota(),
                tenant.getRegularUsers());
        return applyQuotaItemVisibility(item);
    }

    private SpinnerItem applyQuotaItemVisibility(SpinnerItem item) {
        item.setVisible(!tenant.isSystem());
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

        validateSizeQuota(values);

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

        return !vm.hasErrors();
    }

    private void validateEmailAccountsQuota(Map<String, Object> values) {
        if (values.get(EMAILACCOUNTS_QUOTA) == null)
            tenant.setEmailAccountsQuota(null);
        else
            tenant.setEmailAccountsQuota(Long.parseLong(values.get(EMAILACCOUNTS_QUOTA).toString()));
    }

    private void validateImportFoldersQuota(Map<String, Object> values) {
        if (values.get(IMPORTFOLDERS_QUOTA) == null)
            tenant.setImportFoldersQuota(null);
        else
            tenant.setImportFoldersQuota(Long.parseLong(values.get(IMPORTFOLDERS_QUOTA).toString()));
    }

    private void validateStampsQuota(Map<String, Object> values) {
        if (values.get(STAMPS_QUOTA) == null)
            tenant.setStampsQuota(null);
        else
            tenant.setStampsQuota(Long.parseLong(values.get(STAMPS_QUOTA).toString()));
    }

    private void validateReportsQuota(Map<String, Object> values) {
        if (values.get(REPORTS_QUOTA) == null)
            tenant.setReportsQuota(null);
        else
            tenant.setReportsQuota(Long.parseLong(values.get(REPORTS_QUOTA).toString()));
    }

    private void validateFormsQuota(Map<String, Object> values) {
        if (values.get(FORMS_QUOTA) == null)
            tenant.setFormsQuota(null);
        else
            tenant.setFormsQuota(Long.parseLong(values.get(FORMS_QUOTA).toString()));
    }

    private void validateWorkflowsQuota(Map<String, Object> values) {
        if (values.get(WORKFLOWS_QUOTA) == null)
            tenant.setWorkflowsQuota(null);
        else
            tenant.setWorkflowsQuota(Long.parseLong(values.get(WORKFLOWS_QUOTA).toString()));
    }

    private void validateTicketsQuota(Map<String, Object> values) {
        if (values.get(TICKETS_QUOTA) == null)
            tenant.setTicketsQuota(null);
        else
            tenant.setTicketsQuota(Long.parseLong(values.get(TICKETS_QUOTA).toString()));
    }

    private void validateSessionsQuota(Map<String, Object> values) {
        if (values.get(SESSIONS_QUOTA) == null)
            tenant.setSessionsQuota(null);
        else
            tenant.setSessionsQuota(Long.parseLong(values.get(SESSIONS_QUOTA).toString()));
    }

    private void validateUsersQuota(Map<String, Object> values) {
        if (values.get(REGULAR_USERS_QUOTA) == null)
            tenant.setRegularUsersQuota(null);
        else
            tenant.setRegularUsersQuota(Long.parseLong(values.get(REGULAR_USERS_QUOTA).toString()));

        if (values.get(READONLY_USERS_QUOTA) == null)
            tenant.setReadonlyUsersQuota(null);
        else
            tenant.setReadonlyUsersQuota(Long.parseLong(values.get(READONLY_USERS_QUOTA).toString()));
    }

    private void validateApiCallsQuota(Map<String, Object> values) {
        if (values.get(MONTHLYAPICALLS_QUOTA) == null)
            tenant.setApiCallsQuota(null);
        else
            tenant.setApiCallsQuota(Long.parseLong(values.get(MONTHLYAPICALLS_QUOTA).toString()));
    }

    private void validateSizeQuota(Map<String, Object> values) {
        if (values.get(STORAGE_QUOTA) == null)
            tenant.setStorageQuota(null);
        else
            tenant.setStorageQuota(Long.parseLong(values.get(STORAGE_QUOTA).toString()));
    }

    private void validateDocumentsQuota(Map<String, Object> values) {
        if (values.get(DOCUMENTS_QUOTA) == null)
            tenant.setDocumentsQuota(null);
        else
            tenant.setDocumentsQuota(Long.parseLong(values.get(DOCUMENTS_QUOTA).toString()));
    }

    private void validatePagesQuota(Map<String, Object> values) {
        if (values.get(PAGES_QUOTA) == null)
            tenant.setPagesQuota(null);
        else
            tenant.setPagesQuota(Long.parseLong(values.get(PAGES_QUOTA).toString()));
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