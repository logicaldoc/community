package com.logicaldoc.gui.frontend.client.folder.copy;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.frontend.client.folder.FolderDetailTab;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ToggleItem;

/**
 * Shows folder's optional template metadata
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class FolderCopyExtendedPropertiesPanel extends FolderDetailTab {
	private ExtendedPropertiesPanel propertiesPanel;

	private DynamicForm form1 = new DynamicForm();

	public FolderCopyExtendedPropertiesPanel(GUIFolder folder) {
		super(folder, null);
		setWidth100();
		setHeight100();
		setMembersMargin(1);

		ToggleItem locked = ItemFactory.newToggleItem("locked", "templatelocked", folder.getTemplateLocked() == 1);
		locked.setEndRow(true);

		form1 = new DynamicForm();
		form1.setWidth(200);
		form1.setNumCols(1);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setItems(locked);

		propertiesPanel = new ExtendedPropertiesPanel(folder, null, null, true, false, true);
		setMembers(form1, propertiesPanel);
	}

	@Override
	public boolean validate() {
		if (propertiesPanel.validate() && form1.validate()) {
			folder.setTemplateLocked(Boolean.parseBoolean(form1.getValueAsString("locked")) ? 1 : 0);
			return true;
		}
		return false;
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