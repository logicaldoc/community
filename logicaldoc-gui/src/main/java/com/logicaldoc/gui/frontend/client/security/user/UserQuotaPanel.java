package com.logicaldoc.gui.frontend.client.security.user;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
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

	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

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
		vm.clearValues();
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (Boolean.TRUE.equals(contains(form1)))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);

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

		form1.setItems(quota, quotaCount);
		addMember(form1);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values =  vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			long quota;
			if (values.get(QUOTA) instanceof String str)
				quota = Integer.parseInt(str);
			else
				quota = (Integer) values.get(QUOTA);
			if (quota > 0)
				user.setQuota(quota * (1024 * 1024));
			else
				user.setQuota(-1);
		}
		return !vm.hasErrors();
	}
}
