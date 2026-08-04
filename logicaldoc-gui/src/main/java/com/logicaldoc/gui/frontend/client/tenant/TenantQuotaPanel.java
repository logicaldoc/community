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

    private static final String PAGESQUOTA = "pagesquota";

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

    private static final String STAMPSQUOTA = "stampsquota";

    private static final String IMPORTFOLDERSQUOTA = "importfoldersquota";

    private static final String EMAILACCOUNTSQUOTA = "emailaccountsquota";

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

        SpinnerItem sizeQuota = prepareSizeQuotaItem();

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

        StaticTextItem size = ItemFactory.newStaticTextItem("ssize", "size", Util.formatSizeW7(tenant.getSize()));
        size.setWrap(false);
        StaticTextItem documents = ItemFactory.newStaticTextItem("documents", Util.formatLong(tenant.getDocuments()));
        StaticTextItem pages = ItemFactory.newStaticTextItem("pages", Util.formatLong(tenant.getPages()));
        StaticTextItem sessions = ItemFactory.newStaticTextItem("sessions", Util.formatLong(tenant.getSessions()));
        StaticTextItem apicalls = ItemFactory.newStaticTextItem("monthlyapicalls",
                Util.formatLong(tenant.getApiCalls()));
        StaticTextItem regularUsers = ItemFactory.newStaticTextItem("users", Util.formatLong(tenant.getUsers()));
        StaticTextItem readonlyUsers = ItemFactory.newStaticTextItem("guests", "readonlyusers",
                Util.formatLong(tenant.getGuests()));
        StaticTextItem tickets = ItemFactory.newStaticTextItem("tickets", Util.formatLong(tenant.getTickets()));
        StaticTextItem workflows = ItemFactory.newStaticTextItem("workflows", Util.formatLong(tenant.getWorkflows()));
        StaticTextItem forms = ItemFactory.newStaticTextItem("forms", Util.formatLong(tenant.getForms()));
        StaticTextItem reports = ItemFactory.newStaticTextItem("reports", Util.formatLong(tenant.getReports()));
        StaticTextItem stamps = ItemFactory.newStaticTextItem("stamps", Util.formatLong(tenant.getStamps()));
        StaticTextItem importFolders = ItemFactory.newStaticTextItem("importfolders",
                Util.formatLong(tenant.getImportFolders()));
        StaticTextItem emailAccounts = ItemFactory.newStaticTextItem("emailaccounts",
                Util.formatLong(tenant.getEmailAccounts()));

        // Static items to display whole system quotas
        StaticTextItem regularUsersSystemQuota = ItemFactory.newStaticTextItem("sys" + USERSQUOTA, USERSQUOTA,
                Util.formatLong(tenant.getUsersQuota()));
        regularUsersSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem readonlyUsersSystemQuota = ItemFactory.newStaticTextItem("sys" + GUESTSQUOTA, "readonlyusersquota",
                Util.formatLong(tenant.getReadonlyUsersQuota()));
        readonlyUsersSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem sessionsSystemQuota = ItemFactory.newStaticTextItem("sys" + SESSIONSQUOTA, SESSIONSQUOTA,
                Util.formatLong(tenant.getSessionsQuota()));
        sessionsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem documentsSystemQuota = ItemFactory.newStaticTextItem("sys" + DOCUMENTSQUOTA, DOCUMENTSQUOTA,
                Util.formatLong(tenant.getDocumentsQuota()));
        documentsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem pagesSystemQuota = ItemFactory.newStaticTextItem("sys" + PAGESQUOTA, PAGESQUOTA,
                Util.formatLong(tenant.getPagesQuota()));
        pagesSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem sizeSystemQuota = ItemFactory.newStaticTextItem("sys" + SIZEQUOTA, SIZEQUOTA,
                Util.formatSizeW7((double) tenant.getStorageQuota() * 1024D * 1024D));
        sizeSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem monthlyApiCallsSystemQuota = ItemFactory.newStaticTextItem("sys" + MONTHLYAPICALLSQUOTA,
                MONTHLYAPICALLSQUOTA, Util.formatLong(tenant.getApiCallsQuota()));
        monthlyApiCallsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem ticketsSystemQuota = ItemFactory.newStaticTextItem("sys" + TICKETSQUOTA, TICKETSQUOTA,
                Util.formatLong(tenant.getTicketsQuota()));
        ticketsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem workflowsSystemQuota = ItemFactory.newStaticTextItem("sys" + WORKFLOWSQUOTA, WORKFLOWSQUOTA,
                Util.formatLong(tenant.getWorkflowsQuota()));
        workflowsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem formsSystemQuota = ItemFactory.newStaticTextItem("sys" + FORMSQUOTA, FORMSQUOTA,
                Util.formatLong(tenant.getFormsQuota()));
        formsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem reportsSystemQuota = ItemFactory.newStaticTextItem("sys" + REPORTSQUOTA, REPORTSQUOTA,
                Util.formatLong(tenant.getReportsQuota()));
        reportsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem stampsSystemQuota = ItemFactory.newStaticTextItem("sys" + STAMPSQUOTA, STAMPSQUOTA,
                Util.formatLong(tenant.getStampsQuota()));
        stampsSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem importFoldersSystemQuota = ItemFactory.newStaticTextItem("sys" + IMPORTFOLDERSQUOTA,
                IMPORTFOLDERSQUOTA, Util.formatLong(tenant.getImportFoldersQuota()));
        importFoldersSystemQuota.setVisible(tenant.isSystem());
        StaticTextItem emailAccountsSystemQuota = ItemFactory.newStaticTextItem("sys" + EMAILACCOUNTSQUOTA,
                EMAILACCOUNTSQUOTA, Util.formatLong(tenant.getMaxEmailAccounts()));
        emailAccountsSystemQuota.setVisible(tenant.isSystem());

        form.setItems(regularUsersQuota, regularUsersSystemQuota, regularUsers, readonlyUsersQuota,
                readonlyUsersSystemQuota,
                readonlyUsers, sessionsQuota,
                sessionsSystemQuota, sessions, documentsQuota, documentsSystemQuota, documents, pagesQuota, pagesSystemQuota, pages, sizeQuota,
                sizeSystemQuota, size, monthlyApiCallsQuota, monthlyApiCallsSystemQuota, apicalls, ticketsQuota,
                ticketsSystemQuota, tickets, workflowsQuota, workflowsSystemQuota, workflows, formsSystemQuota,
                formsQuota, forms, reportsSystemQuota, reportsQuota, reports, stampsSystemQuota, stampsQuota, stamps,
                importFoldersSystemQuota, importFoldersQuota, importFolders, emailAccountsSystemQuota,
                emailAccountsQuota, emailAccounts, quotaThreshold, recipients);
        addMember(layout);
    }

    private SpinnerItem prepareEmailAccountsQuotaItem() {
        SpinnerItem emailAccountsQuota = ItemFactory.newSpinnerItem(EMAILACCOUNTSQUOTA, tenant.getMaxEmailAccounts());
        emailAccountsQuota.setDisabled(changedHandler == null);
        emailAccountsQuota.setRequired(false);
        emailAccountsQuota.setMin(-1);
        emailAccountsQuota.setStep(1);
        emailAccountsQuota.setWidth(120);
        emailAccountsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            emailAccountsQuota.addChangedHandler(changedHandler);
        return emailAccountsQuota;
    }

    private SpinnerItem prepareImportFoldersQuotaItem() {
        SpinnerItem importFoldersQuota = ItemFactory.newSpinnerItem(IMPORTFOLDERSQUOTA, tenant.getImportFoldersQuota());
        importFoldersQuota.setDisabled(changedHandler == null);
        importFoldersQuota.setRequired(false);
        importFoldersQuota.setMin(-1);
        importFoldersQuota.setStep(1);
        importFoldersQuota.setWidth(120);
        importFoldersQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            importFoldersQuota.addChangedHandler(changedHandler);
        return importFoldersQuota;
    }

    private SpinnerItem prepareStampsQuotaItem() {
        SpinnerItem stampsQuota = ItemFactory.newSpinnerItem(STAMPSQUOTA, tenant.getStampsQuota());
        stampsQuota.setDisabled(changedHandler == null);
        stampsQuota.setRequired(false);
        stampsQuota.setMin(-1);
        stampsQuota.setStep(1);
        stampsQuota.setWidth(120);
        stampsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            stampsQuota.addChangedHandler(changedHandler);
        return stampsQuota;
    }

    private SpinnerItem prepareReportsQuotaItem() {
        SpinnerItem reportsQuota = ItemFactory.newSpinnerItem(REPORTSQUOTA, tenant.getReportsQuota());
        reportsQuota.setDisabled(changedHandler == null);
        reportsQuota.setRequired(false);
        reportsQuota.setMin(-1);
        reportsQuota.setStep(1);
        reportsQuota.setWidth(120);
        reportsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            reportsQuota.addChangedHandler(changedHandler);
        return reportsQuota;
    }

    private SpinnerItem prepareFormsQuotaItem() {
        SpinnerItem formsQuota = ItemFactory.newSpinnerItem(FORMSQUOTA, tenant.getFormsQuota());
        formsQuota.setDisabled(changedHandler == null);
        formsQuota.setRequired(false);
        formsQuota.setMin(-1);
        formsQuota.setStep(1);
        formsQuota.setWidth(120);
        formsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            formsQuota.addChangedHandler(changedHandler);
        return formsQuota;
    }

    private SpinnerItem prepareWorkflowQuotaItem() {
        SpinnerItem workflowsQuota = ItemFactory.newSpinnerItem(WORKFLOWSQUOTA, tenant.getWorkflowsQuota());
        workflowsQuota.setDisabled(changedHandler == null);
        workflowsQuota.setRequired(false);
        workflowsQuota.setMin(-1);
        workflowsQuota.setStep(1);
        workflowsQuota.setWidth(120);
        workflowsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            workflowsQuota.addChangedHandler(changedHandler);
        return workflowsQuota;
    }

    private SpinnerItem prepareTicketsQuoteItem() {
        SpinnerItem ticketsQuota = ItemFactory.newSpinnerItem(TICKETSQUOTA, tenant.getTicketsQuota());
        ticketsQuota.setDisabled(changedHandler == null);
        ticketsQuota.setRequired(false);
        ticketsQuota.setMin(-1);
        ticketsQuota.setStep(10);
        ticketsQuota.setWidth(120);
        ticketsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            ticketsQuota.addChangedHandler(changedHandler);
        return ticketsQuota;
    }

    private SpinnerItem prepareSizeQuotaItem() {
        SpinnerItem sizeQuota = ItemFactory.newSpinnerItem(SIZEQUOTA, tenant.getStorageQuota());
        sizeQuota.setHint("MB");
        sizeQuota.setDisabled(changedHandler == null);
        sizeQuota.setRequired(false);
        sizeQuota.setMin(-1);
        sizeQuota.setStep(1024);
        sizeQuota.setWidth(120);
        sizeQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            sizeQuota.addChangedHandler(changedHandler);
        return sizeQuota;
    }

    private SpinnerItem prepareDocumentsQuotaItem() {
        SpinnerItem documentsQuota = ItemFactory.newSpinnerItem(DOCUMENTSQUOTA, tenant.getDocumentsQuota());
        documentsQuota.setDisabled(changedHandler == null);
        documentsQuota.setRequired(false);
        documentsQuota.setMin(-1);
        documentsQuota.setStep(10000);
        documentsQuota.setWidth(120);
        documentsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            documentsQuota.addChangedHandler(changedHandler);
        return documentsQuota;
    }

    private SpinnerItem preparePagesQuotaItem() {
        SpinnerItem pagesQuota = ItemFactory.newSpinnerItem(PAGESQUOTA, tenant.getPagesQuota());
        pagesQuota.setDisabled(changedHandler == null);
        pagesQuota.setRequired(false);
        pagesQuota.setWidth(120);
        pagesQuota.setMin(-1);
        pagesQuota.setStep(10);
        pagesQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            pagesQuota.addChangedHandler(changedHandler);
        return pagesQuota;
    }

    private SpinnerItem prepareMonthlyApiCallsQuotaItem() {
        SpinnerItem monthlyApiCallsQuota = ItemFactory.newSpinnerItem(MONTHLYAPICALLSQUOTA, tenant.getApiCallsQuota());
        monthlyApiCallsQuota.setDisabled(changedHandler == null);
        monthlyApiCallsQuota.setRequired(false);
        monthlyApiCallsQuota.setMin(-1);
        monthlyApiCallsQuota.setStep(10);
        monthlyApiCallsQuota.setWidth(120);
        monthlyApiCallsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            monthlyApiCallsQuota.addChangedHandler(changedHandler);
        return monthlyApiCallsQuota;
    }

    private SpinnerItem prepareSessionsQuotaItem() {
        SpinnerItem sessionsQuota = ItemFactory.newSpinnerItem(SESSIONSQUOTA, tenant.getSessionsQuota());
        sessionsQuota.setDisabled(changedHandler == null);
        sessionsQuota.setRequired(false);
        sessionsQuota.setMin(-1);
        sessionsQuota.setStep(1);
        sessionsQuota.setWidth(120);
        sessionsQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            sessionsQuota.addChangedHandler(changedHandler);
        return sessionsQuota;
    }

    private SpinnerItem prepareReadonlyUsersQuotaItem() {
        SpinnerItem readonlyUsersQuota = ItemFactory.newSpinnerItem(GUESTSQUOTA, "readonlyusersquota",
                tenant.getReadonlyUsersQuota());
        readonlyUsersQuota.setDisabled(changedHandler == null);
        readonlyUsersQuota.setRequired(false);
        readonlyUsersQuota.setMin(-1);
        readonlyUsersQuota.setStep(1);
        readonlyUsersQuota.setWidth(80);
        readonlyUsersQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            readonlyUsersQuota.addChangedHandler(changedHandler);
        return readonlyUsersQuota;
    }

    private SpinnerItem prepareRegularUsersQuotaItem() {
        SpinnerItem regularUsersQuota = ItemFactory.newSpinnerItem(USERSQUOTA, tenant.getUsersQuota());
        regularUsersQuota.setDisabled(changedHandler == null);
        regularUsersQuota.setRequired(false);
        regularUsersQuota.setMin(1);
        regularUsersQuota.setStep(1);
        regularUsersQuota.setWidth(80);
        regularUsersQuota.setVisible(!tenant.isSystem());
        if (changedHandler != null)
            regularUsersQuota.addChangedHandler(changedHandler);
        return regularUsersQuota;
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

        validatereportsQuota(values);

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
        if (values.get(EMAILACCOUNTSQUOTA) == null)
            tenant.setMaxEmailAccounts(null);
        else
            tenant.setMaxEmailAccounts(Long.parseLong(values.get(EMAILACCOUNTSQUOTA).toString()));
    }

    private void validateImportFoldersQuota(Map<String, Object> values) {
        if (values.get(IMPORTFOLDERSQUOTA) == null)
            tenant.setImportFoldersQuota(null);
        else
            tenant.setImportFoldersQuota(Long.parseLong(values.get(IMPORTFOLDERSQUOTA).toString()));
    }

    private void validateStampsQuota(Map<String, Object> values) {
        if (values.get(STAMPSQUOTA) == null)
            tenant.setStampsQuota(null);
        else
            tenant.setStampsQuota(Long.parseLong(values.get(STAMPSQUOTA).toString()));
    }

    private void validatereportsQuota(Map<String, Object> values) {
        if (values.get(REPORTSQUOTA) == null)
            tenant.setReportsQuota(null);
        else
            tenant.setReportsQuota(Long.parseLong(values.get(REPORTSQUOTA).toString()));
    }

    private void validateFormsQuota(Map<String, Object> values) {
        if (values.get(FORMSQUOTA) == null)
            tenant.setFormsQuota(null);
        else
            tenant.setFormsQuota(Long.parseLong(values.get(FORMSQUOTA).toString()));
    }

    private void validateWorkflowsQuota(Map<String, Object> values) {
        if (values.get(WORKFLOWSQUOTA) == null)
            tenant.setWorkflowsQuota(null);
        else
            tenant.setWorkflowsQuota(Long.parseLong(values.get(WORKFLOWSQUOTA).toString()));
    }

    private void validateTicketsQuota(Map<String, Object> values) {
        if (values.get(TICKETSQUOTA) == null)
            tenant.setTicketsQuota(null);
        else
            tenant.setTicketsQuota(Long.parseLong(values.get(TICKETSQUOTA).toString()));
    }

    private void validateSessionsQuota(Map<String, Object> values) {
        if (values.get(SESSIONSQUOTA) == null)
            tenant.setSessionsQuota(null);
        else
            tenant.setSessionsQuota(Long.parseLong(values.get(SESSIONSQUOTA).toString()));
    }

    private void validateUsersQuota(Map<String, Object> values) {
        if (values.get(USERSQUOTA) == null)
            tenant.setRegularUsersQuota(null);
        else
            tenant.setRegularUsersQuota(Long.parseLong(values.get(USERSQUOTA).toString()));

        if (values.get(GUESTSQUOTA) == null)
            tenant.setReadonlyUsersQuota(null);
        else
            tenant.setReadonlyUsersQuota(Long.parseLong(values.get(GUESTSQUOTA).toString()));
    }

    private void validateApiCallsQuota(Map<String, Object> values) {
        if (values.get(MONTHLYAPICALLSQUOTA) == null)
            tenant.setApiCallsQuota(null);
        else
            tenant.setApiCallsQuota(Long.parseLong(values.get(MONTHLYAPICALLSQUOTA).toString()));
    }

    private void validateSizeQuota(Map<String, Object> values) {
        if (values.get(SIZEQUOTA) == null)
            tenant.setStorageQuota(null);
        else
            tenant.setStorageQuota(Long.parseLong(values.get(SIZEQUOTA).toString()));
    }

    private void validateDocumentsQuota(Map<String, Object> values) {
        if (values.get(DOCUMENTSQUOTA) == null)
            tenant.setDocumentsQuota(null);
        else
            tenant.setDocumentsQuota(Long.parseLong(values.get(DOCUMENTSQUOTA).toString()));
    }
    
    private void validatePagesQuota(Map<String, Object> values) {
        if (values.get(PAGESQUOTA) == null)
            tenant.setPagesQuota(null);
        else
            tenant.setPagesQuota(Long.parseLong(values.get(PAGESQUOTA).toString()));
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