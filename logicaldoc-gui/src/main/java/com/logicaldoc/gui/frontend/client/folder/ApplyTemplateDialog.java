package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * This is the form used to apply a folder template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class ApplyTemplateDialog extends Dialog {

	public ApplyTemplateDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("applytemplate"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		VLayout content = new VLayout();
		content.setWidth100();
		content.setHeight100();
		content.setMembersMargin(2);

		final TreeGrid folders = new FolderTree();
		folders.setWidth100();
		folders.setHeight100();

		final DynamicForm form = new DynamicForm();

		CheckboxItem inheritSecurity = new CheckboxItem();
		inheritSecurity.setName("inheritSecurity");
		inheritSecurity.setTitle(I18N.message("inheritparentsec"));
		inheritSecurity.setValue("inherit".equals(Session.get().getConfig("gui.security.inheritoption.default")));
		inheritSecurity.setHidden(!Session.get().getConfigAsBoolean("gui.security.inheritoption"));

		SelectItem templateSelector = ItemFactory.newFolderTemplateSelector();

		form.setItems(templateSelector, inheritSecurity);

		Button apply = new Button(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.setMargin(1);
		apply.addClickHandler(event -> {
			if (!form.validate())
				return;

			final TreeNode selectedNode = FolderNavigator.get().getSelectedRecord();
			final long folderId = Long.parseLong(selectedNode.getAttributeAsString("folderId"));
			long templateId = Long.parseLong(form.getValueAsString("foldertemplate"));

			LD.contactingServer();

			FolderService.Instance.get().applyTemplate(folderId, templateId,
					Boolean.valueOf(form.getValueAsString("inheritSecurity")), new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void arg0) {
							LD.clearPrompt();
							FolderNavigator.get().getTree().reloadChildren(selectedNode);
							GuiLog.info(I18N.message("templateapplied"), null);
							destroy();
						}
					});
		});

		content.setMembers(form, apply);
		addItem(content);
	}
}