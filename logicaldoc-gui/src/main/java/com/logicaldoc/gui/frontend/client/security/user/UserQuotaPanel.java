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

	private static final String MAX_CONCURRENT_SESSIONS = "maxconcurrentsessions";

	private static final String MAX_STORAGE = "maxstorage";

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

		SpinnerItem maxStorage = ItemFactory.newSpinnerItem(MAX_STORAGE, MAX_STORAGE, (Integer) null);
		maxStorage.setRequired(true);
		maxStorage.setWidth(120);
		maxStorage.setMin(-1);
		maxStorage.setStep(10);
		maxStorage.setValue(user.getQuota() >= 0 ? user.getQuota() / (1024 * 1024) : -1);
		maxStorage.setHint("MB");
		if (!readonly)
			maxStorage.addChangedHandler(changedHandler);

		StaticTextItem storageUsage = ItemFactory.newStaticTextItem("storageUsage", "usage",
				Util.formatSizeW7(user.getQuotaCount()));
		storageUsage.setWrap(false);

		SpinnerItem maxConcurrentSessions = ItemFactory.newSpinnerItem(MAX_CONCURRENT_SESSIONS, MAX_CONCURRENT_SESSIONS,
				(Integer) null);
		maxConcurrentSessions.setRequired(true);
		maxConcurrentSessions.setWidth(120);
		maxConcurrentSessions.setMin(-1);
		maxConcurrentSessions.setStep(10);
		maxConcurrentSessions.setValue(user.getSessionsQuota());
		if (!readonly)
			maxConcurrentSessions.addChangedHandler(changedHandler);

		StaticTextItem sessionsUsage = ItemFactory.newStaticTextItem("sessionsUsage", "usage",
				Long.toString(user.getSessionsQuotaCount()));
		sessionsUsage.setWrap(false);

		form.setItems(maxStorage, storageUsage, maxConcurrentSessions, sessionsUsage);
		addMember(form);
	}

	boolean validate() {
		if (form.validate()) {
			long maxRepoSize = Long.parseLong(form.getValueAsString(MAX_STORAGE));
			if (maxRepoSize > 0)
				user.setQuota(maxRepoSize * (1024 * 1024));
			else
				user.setQuota(-1);

			long maxSessions = Long.parseLong(form.getValueAsString(MAX_CONCURRENT_SESSIONS));
			if (maxSessions > 0)
				user.setSessionsQuota(maxSessions);
			else
				user.setSessionsQuota(-1);
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