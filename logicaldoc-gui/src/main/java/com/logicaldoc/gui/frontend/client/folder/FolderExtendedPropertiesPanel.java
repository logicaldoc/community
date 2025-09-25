package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows folder's optional template metadata
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderExtendedPropertiesPanel extends FolderDetailTab {
	private ExtendedPropertiesPanel propertiesPanel;

	private DynamicForm form1 = new DynamicForm();

	public FolderExtendedPropertiesPanel(GUIFolder folder, ChangedHandler changedHandler,
			ChangedHandler templateChangedHandler) {
		super(folder, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(1);

		ToggleItem locked = ItemFactory.newToggleItem("locked", "templatelocked", folder.getTemplateLocked() == 1);
		locked.addChangedHandler(changedHandler);
		locked.setEndRow(true);

		ButtonItem applyMetadata = new ButtonItem(I18N.message("applytosubfolders"));
		applyMetadata.setAutoFit(true);
		applyMetadata.setEndRow(true);
		applyMetadata.setDisabled(!folder.isWrite());
		applyMetadata.setColSpan(1);
		applyMetadata.addClickHandler(event -> {
			LD.contactingServer();
			FolderService.Instance.get().applyMetadata(folder.getId(), new DefaultAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					super.onFailure(caught);
				}

				@Override
				public void onSuccess(Void v) {
					LD.clearPrompt();
				}
			});
		});

		form1 = new DynamicForm();
		form1.setWidth(200);
		form1.setNumCols(1);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setItems(locked, applyMetadata);

		propertiesPanel = new ExtendedPropertiesPanel(folder, changedHandler, templateChangedHandler, folder.isWrite(),
				false, true);
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
	public void handleErrors(ServerValidationError[] errors) {
		propertiesPanel.onErrors(errors);
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