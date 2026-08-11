package com.logicaldoc.gui.frontend.client.security.user;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows user's quota settings and values.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class UserQuotaPanel extends HLayout {

    private static final String QUOTA_THRESHOLD = "quotaThreshold";

    private static final String PAGES_QUOTA = "pagesquota";

    private static final String DOCUMENTS_QUOTA = "documentsquota";

    private static final String SESSIONS_QUOTA = "sessionsquota";

    private static final String STORAGE_QUOTA = "storagequota";

    private DynamicForm form = new DynamicForm();

    private GUIUser user;

    private ChangedHandler changedHandler;

    public UserQuotaPanel(GUIUser user, ChangedHandler changedHandler) {
        this.user = user;
        this.changedHandler = changedHandler;

        setWidth100();
        setHeight100();
        setMembersMargin(20);
        refresh();
    }

    private void refresh() {
        boolean readonly = (changedHandler == null);
        form.clearValues();
        form.clearErrors(false);
        form.destroy();

        if (Boolean.TRUE.equals(contains(form)))
            removeChild(form);
        form = new DynamicForm();
        form.setTitleOrientation(TitleOrientation.TOP);

        SpinnerItem storageQuota = ItemFactory.newQuotaSpinnerItem(STORAGE_QUOTA, user.getStorageQuota(),
                user.getStorage());
        storageQuota.setHint("MB " + I18N.message("usedhint", Util.formatSizeW7(user.getStorage())));
        storageQuota.setStep(1024);
        storageQuota.setDisabled(readonly);
        if (!readonly)
            storageQuota.addChangedHandler(changedHandler);

        SpinnerItem sessionsQuota = ItemFactory.newQuotaSpinnerItem(SESSIONS_QUOTA, user.getSessionsQuota(),
                user.getSessions());
        sessionsQuota.setDisabled(readonly);
        if (!readonly)
            sessionsQuota.addChangedHandler(changedHandler);

        SpinnerItem pagesQuota = ItemFactory.newQuotaSpinnerItem(PAGES_QUOTA, user.getPagesQuota(), user.getPages());
        pagesQuota.setStep(1000);
        pagesQuota.setDisabled(readonly);
        if (!readonly)
            pagesQuota.addChangedHandler(changedHandler);

        SpinnerItem documentsQuota = ItemFactory.newQuotaSpinnerItem(DOCUMENTS_QUOTA, user.getDocumentsQuota(),
                user.getDocuments());
        documentsQuota.setStep(1000);
        documentsQuota.setDisabled(readonly);
        if (!readonly)
            documentsQuota.addChangedHandler(changedHandler);

        SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
                user.getQuotaThreshold());
        quotaThreshold.setMax(100);
        quotaThreshold.setMin(0);
        quotaThreshold.setHint("%");
        if (!readonly)
            quotaThreshold.addChangedHandler(changedHandler);

        form.setItems(documentsQuota, pagesQuota, storageQuota, sessionsQuota, quotaThreshold);
        addMember(form);
    }

    boolean validate() {
        if (form.validate()) {
            String val = form.getValueAsString(STORAGE_QUOTA);
            if (val != null)
                user.setStorageQuota(Long.parseLong(form.getValueAsString(STORAGE_QUOTA)));
            else
                user.setStorageQuota(null);

            val = form.getValueAsString(DOCUMENTS_QUOTA);
            if (val != null)
                user.setDocumentsQuota(Long.parseLong(form.getValueAsString(DOCUMENTS_QUOTA)));
            else
                user.setDocumentsQuota(null);

            val = form.getValueAsString(PAGES_QUOTA);
            if (val != null)
                user.setPagesQuota(Long.parseLong(form.getValueAsString(PAGES_QUOTA)));
            else
                user.setPagesQuota(null);

            val = form.getValueAsString(SESSIONS_QUOTA);
            if (val != null)
                user.setSessionsQuota(Long.parseLong(form.getValueAsString(SESSIONS_QUOTA)));
            else
                user.setSessionsQuota(null);

            if (form.getValueAsString(QUOTA_THRESHOLD) == null)
                user.setQuotaThreshold(null);
            else
                user.setQuotaThreshold(Integer.parseInt(form.getValueAsString(QUOTA_THRESHOLD)));

        }
        return !form.hasErrors();
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