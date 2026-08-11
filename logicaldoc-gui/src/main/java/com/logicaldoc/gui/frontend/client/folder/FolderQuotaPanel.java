package com.logicaldoc.gui.frontend.client.folder;

import java.util.Map;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows the folder's quotas
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.4.3
 */
public class FolderQuotaPanel extends FolderDetailTab {

    private static final String QUOTA_THRESHOLD = "quotaThreshold";

    private static final String PAGES_QUOTA = "pagesquota";

    private static final String STORAGE_QUOTA = "storagequota";

    private static final String DOCUMENTS_QUOTA = "documentsquota";

    private DynamicForm form = new DynamicForm();

    private ValuesManager vm = new ValuesManager();

    private boolean update = false;

    private MultiComboBoxItem recipients;

    public FolderQuotaPanel(GUIFolder folder, ChangedHandler changedHandler) {
        super(folder, changedHandler);
        setWidth100();
        setHeight100();
        setMembersMargin(20);
        update = folder.isWorkspace() && Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN);
        refresh();
    }

    private void refresh() {
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

        SpinnerItem documentsQuota = ItemFactory.newQuotaSpinnerItem(DOCUMENTS_QUOTA, folder.getDocumentsQuota(),
                folder.getDocuments());
        documentsQuota.setStep(1000);
        documentsQuota.setDisabled(!update);
        if (update)
            documentsQuota.addChangedHandler(changedHandler);

        SpinnerItem pagesQuota = ItemFactory.newQuotaSpinnerItem(PAGES_QUOTA, folder.getPagesQuota(),
                folder.getPages());
        pagesQuota.setStep(1000);
        pagesQuota.setDisabled(!update);
        if (update)
            pagesQuota.addChangedHandler(changedHandler);

        SpinnerItem storageQuota = ItemFactory.newQuotaSpinnerItem(STORAGE_QUOTA, folder.getStorageQuota(),
                folder.getStorage());
        storageQuota.setHint("MB " + I18N.message("usedhint", Util.formatSizeW7(folder.getStorage())));
        storageQuota.setStep(1024);
        storageQuota.setEndRow(true);
        storageQuota.setDisabled(!update);
        if (update)
            storageQuota.addChangedHandler(changedHandler);

        SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
                folder.getQuotaThreshold());
        quotaThreshold.setDisabled(!update);
        quotaThreshold.setMax(100);
        quotaThreshold.setMin(0);
        quotaThreshold.setHint("%");
        if (update)
            quotaThreshold.addChangedHandler(changedHandler);

        recipients = ItemFactory.newMultiComboBoxItem("recipients", "alertrecipients", new UsersDS(null, false, false),
                folder.getQuotaAlertRecipients().toArray(new String[0]));
        recipients.setValueField("username");
        recipients.setDisplayField("username");
        if (update)
            recipients.addChangedHandler(changedHandler);

        documentsQuota.setDisabled(!update);
        storageQuota.setDisabled(!update);
        quotaThreshold.setDisabled(!update);
        recipients.setDisabled(!update);

        form.setItems(documentsQuota, pagesQuota, storageQuota, quotaThreshold, recipients);
        addMember(form);
    }

    @Override
    public boolean validate() {
        @SuppressWarnings("unchecked")
        Map<String, Object> values = vm.getValues();

        vm.validate();
        if (Boolean.TRUE.equals(vm.hasErrors()))
            return false;

        if (values.get(DOCUMENTS_QUOTA) == null)
            folder.setDocumentsQuota(null);
        else
            folder.setDocumentsQuota(Long.parseLong(values.get(DOCUMENTS_QUOTA).toString()));

        if (values.get(PAGES_QUOTA) == null)
            folder.setPagesQuota(null);
        else
            folder.setPagesQuota(Long.parseLong(values.get(PAGES_QUOTA).toString()));

        if (values.get(STORAGE_QUOTA) == null)
            folder.setStorageQuota(null);
        else
            folder.setStorageQuota(Long.parseLong(values.get(STORAGE_QUOTA).toString()));

        if (values.get(QUOTA_THRESHOLD) == null)
            folder.setQuotaThreshold(null);
        else
            folder.setQuotaThreshold(Integer.parseInt(values.get(QUOTA_THRESHOLD).toString()));

        folder.clearQuotaAlertRecipients();
        String[] usernames = recipients.getValues();
        if (usernames != null && usernames.length > 0)
            for (int i = 0; i < usernames.length; i++)
                folder.addQuotaAlertRecipient(usernames[i]);

        return !vm.hasErrors();
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