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

        SpinnerItem storageQuota = ItemFactory.newSpinnerItem(STORAGEQUOTA, STORAGEQUOTA, (Integer) null);
        storageQuota.setRequired(true);
        storageQuota.setWidth(120);
        storageQuota.setMin(-1);
        storageQuota.setStep(10);
        storageQuota.setValue(user.getStorageQuota() >= 0 ? user.getStorageQuota() / (1024 * 1024) : -1);
        storageQuota.setHint("MB");
        if (!readonly)
            storageQuota.addChangedHandler(changedHandler);

        StaticTextItem storageUsage = ItemFactory.newStaticTextItem("storageUsage", "usage",
                Util.formatSizeW7(user.getStorage()));
        storageUsage.setWrap(false);

        StaticTextItem sessionsUsage = ItemFactory.newStaticTextItem("sessionsUsage", "usage",
                Long.toString(user.getSessions()));
        sessionsUsage.setWrap(false);

        SpinnerItem sessionsQuota = ItemFactory.newSpinnerItem(SESSIONSQUOTA, SESSIONSQUOTA, (Long) null);
        sessionsQuota.setRequired(true);
        sessionsQuota.setWidth(120);
        sessionsQuota.setMin(-1);
        sessionsQuota.setStep(1);
        sessionsQuota.setValue(user.getSessionsQuota());
        if (!readonly)
            sessionsQuota.addChangedHandler(changedHandler);

        StaticTextItem pagesUsage = ItemFactory.newStaticTextItem("pagesUsage", "usage",
                Long.toString(user.getPages()));
        pagesUsage.setWrap(false);

        SpinnerItem pagesQuota = ItemFactory.newSpinnerItem(PAGESQUOTA, PAGESQUOTA, (Long) null);
        pagesQuota.setWidth(120);
        pagesQuota.setMin(-1);
        pagesQuota.setStep(10);
        pagesQuota.setValue(user.getPages());
        if (!readonly)
            pagesQuota.addChangedHandler(changedHandler);

        StaticTextItem documentsUsage = ItemFactory.newStaticTextItem("documentsUsage", "usage",
                Long.toString(user.getDocuments()));
        documentsUsage.setWrap(false);

        SpinnerItem documentsQuota = ItemFactory.newSpinnerItem(DOCUMENTSQUOTA, DOCUMENTSQUOTA, (Long) null);
        documentsQuota.setWidth(120);
        documentsQuota.setMin(-1);
        documentsQuota.setStep(10);
        documentsQuota.setValue(user.getDocuments());
        if (!readonly)
            documentsQuota.addChangedHandler(changedHandler);

        SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem("alertthreshold", user.getQuotaThreshold());
        quotaThreshold.setMax(100);
        quotaThreshold.setMin(0);
        quotaThreshold.setHint("%");
        if (!readonly)
            quotaThreshold.addChangedHandler(changedHandler);

        form.setItems(storageQuota, storageUsage, documentsQuota, documentsUsage, pagesQuota, pagesUsage, sessionsQuota,
                sessionsUsage, quotaThreshold);
        addMember(form);
    }

    boolean validate() {
        if (form.validate()) {
            String val = form.getValueAsString(STORAGEQUOTA);
            if (val != null)
                user.setStorageQuota(Long.parseLong(form.getValueAsString(STORAGEQUOTA)) * 1024 * 1024);
            else
                user.setStorageQuota(null);

            val = form.getValueAsString(DOCUMENTSQUOTA);
            if (val != null)
                user.setSessionsQuota(Long.parseLong(form.getValueAsString(SESSIONSQUOTA)));
            else
                user.setSessionsQuota(null);

            val = form.getValueAsString(DOCUMENTSQUOTA);
            if (val != null)
                user.setSessionsQuota(Long.parseLong(form.getValueAsString(DOCUMENTSQUOTA)));
            else
                user.setDocumentsQuota(null);

            val = form.getValueAsString(PAGESQUOTA);
            if (val != null)
                user.setPagesQuota(Long.parseLong(form.getValueAsString(PAGESQUOTA)));
            else
                user.setPagesQuota(null);

            if (form.getValueAsString("alertthreshold") == null)
                user.setQuotaThreshold(null);
            else
                user.setQuotaThreshold(Integer.parseInt(form.getValueAsString("alertthreshold")));

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