package com.logicaldoc.gui.frontend.client.security.user;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows user's quota settings and values.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class UserQuotaPanel extends HLayout {

    private static final String QUOTA_THRESHOLD = "quotathreshold";

    private static final String USAGE = "usage";

    private static final String PAGESQUOTA = "pagesquota";

    private static final String DOCUMENTSQUOTA = "documentsquota";

    private static final String SESSIONSQUOTA = "sessionsquota";

    private static final String STORAGEQUOTA = "storagequota";

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

        StaticTextItem storage = ItemFactory.newStaticTextItem("storage", USAGE, Util.formatSizeW7(user.getStorage()));
        storage.setWrap(false);

        StaticTextItem sessions = ItemFactory.newStaticTextItem("sessions", USAGE, Long.toString(user.getSessions()));
        sessions.setWrap(false);

        StaticTextItem pages = ItemFactory.newStaticTextItem("pages", USAGE, Long.toString(user.getPages()));
        pages.setWrap(false);

        StaticTextItem documents = ItemFactory.newStaticTextItem("documents", USAGE,
                Long.toString(user.getDocuments()));
        documents.setWrap(false);
        
        SpinnerItem storageQuota = ItemFactory.newSpinnerItem(STORAGEQUOTA, user.getStorageQuota());
        storageQuota.setWidth(120);
        storageQuota.setMin(-1);
        storageQuota.setHint("MB");
        storageQuota.setStep(10);
        if (!readonly)
            storageQuota.addChangedHandler(changedHandler);

        SpinnerItem sessionsQuota = ItemFactory.newSpinnerItem(SESSIONSQUOTA, user.getSessionsQuota());
        sessionsQuota.setWidth(120);
        sessionsQuota.setMin(-1);
        sessionsQuota.setStep(1);
        if (!readonly)
            sessionsQuota.addChangedHandler(changedHandler);

        SpinnerItem pagesQuota = ItemFactory.newSpinnerItem(PAGESQUOTA, user.getPagesQuota());
        pagesQuota.setWidth(120);
        pagesQuota.setMin(-1);
        pagesQuota.setStep(10);
        if (!readonly)
            pagesQuota.addChangedHandler(changedHandler);

        SpinnerItem documentsQuota = ItemFactory.newSpinnerItem(DOCUMENTSQUOTA, user.getDocumentsQuota());
        documentsQuota.setWidth(120);
        documentsQuota.setMin(-1);
        documentsQuota.setStep(10);
        if (!readonly)
            documentsQuota.addChangedHandler(changedHandler);

        SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem(QUOTA_THRESHOLD, "alertthreshold",
                user.getQuotaThreshold());
        quotaThreshold.setMax(100);
        quotaThreshold.setMin(0);
        quotaThreshold.setHint("%");
        if (!readonly)
            quotaThreshold.addChangedHandler(changedHandler);

        form.setItems(documentsQuota, documents, pagesQuota, pages, storageQuota, storage, sessionsQuota, sessions,
                quotaThreshold);
        addMember(form);
    }

    boolean validate() {
        if (form.validate()) {
            String val = form.getValueAsString(STORAGEQUOTA);
            if (val != null)
                user.setStorageQuota(Long.parseLong(form.getValueAsString(STORAGEQUOTA)));
            else
                user.setStorageQuota(null);

            val = form.getValueAsString(DOCUMENTSQUOTA);
            if (val != null)
                user.setDocumentsQuota(Long.parseLong(form.getValueAsString(DOCUMENTSQUOTA)));
            else
                user.setDocumentsQuota(null);

            val = form.getValueAsString(PAGESQUOTA);
            if (val != null)
                user.setPagesQuota(Long.parseLong(form.getValueAsString(PAGESQUOTA)));
            else
                user.setPagesQuota(null);

            val = form.getValueAsString(SESSIONSQUOTA);
            if (val != null)
                user.setSessionsQuota(Long.parseLong(form.getValueAsString(SESSIONSQUOTA)));
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