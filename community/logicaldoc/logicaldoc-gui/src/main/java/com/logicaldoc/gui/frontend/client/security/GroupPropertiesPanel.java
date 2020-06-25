package com.logicaldoc.gui.frontend.client.security;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows group's standard properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupPropertiesPanel extends HLayout {
	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIGroup group;

	private ChangedHandler changedHandler;

	public GroupPropertiesPanel(GUIGroup group, ChangedHandler changedHandler) {
		if (group == null) {
			setMembers(GroupsPanel.SELECT_GROUP);
		} else {
			this.group = group;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);
			refresh();
		}
	}

	private void refresh() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (contains(form1))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", "id", Long.toString(group.getId()));

		TextItem name = ItemFactory.newSimpleTextItem("name", "name", group.getName());
		if (readonly || group.getId() != 0) {
			// In case of already existing group we do not need to enforce any
			// validation
			name = ItemFactory.newTextItem("name", "name", group.getName());
			name.setDisabled(true);
		}
		name.setRequired(true);
		if (!readonly)
			name.addChangedHandler(changedHandler);

		TextItem description = ItemFactory.newTextItem("description", "description", group.getDescription());
		description.setDisabled(readonly);
		if (!readonly)
			description.addChangedHandler(changedHandler);

		SelectItem inherit = ItemFactory.newGroupSelector("inherit", "inheritgroup");
		inherit.setVisible(!readonly);
		if (!readonly)
			inherit.addChangedHandler(changedHandler);

		form1.setItems(id, name, description, inherit);
		addMember(form1);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			group.setDescription((String) values.get("description"));
			group.setName((String) values.get("name"));
			if (values.get("inherit") != null)
				group.setInheritGroupId(Long.parseLong((String) values.get("inherit")));
			else
				group.setInheritGroupId(null);
		}
		return !vm.hasErrors();
	}
}