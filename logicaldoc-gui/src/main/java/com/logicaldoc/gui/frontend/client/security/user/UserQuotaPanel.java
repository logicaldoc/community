package com.logicaldoc.gui.frontend.client.security.user;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
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
	private static final String QUOTA = "quota";

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

		if (form != null)
			form.destroy();

		if (Boolean.TRUE.equals(contains(form)))
			removeChild(form);
		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);

		IntegerItem quota = ItemFactory.newIntegerItem(QUOTA, QUOTA, null);
		quota.setRequired(true);
		quota.setWidth(120);
		quota.setValue(user.getQuota() >= 0 ? user.getQuota() / (1024 * 1024) : -1);
		quota.setHint("MB");
		if (!readonly)
			quota.addChangedHandler(changedHandler);

		StaticTextItem quotaCount = ItemFactory.newStaticTextItem("quotaCount",
				Util.formatSizeW7(user.getQuotaCount()));
		quotaCount.setWrap(false);

		form.setItems(quota, quotaCount);
		addMember(form);
	}

	boolean validate() {
		if (form.validate()) {
			long quota;
			if (form.getValue(QUOTA) instanceof String str)
				quota = Integer.parseInt(str);
			else
				quota = (Integer) form.getValue(QUOTA);
			if (quota > 0)
				user.setQuota(quota * (1024 * 1024));
			else
				user.setQuota(-1);
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
