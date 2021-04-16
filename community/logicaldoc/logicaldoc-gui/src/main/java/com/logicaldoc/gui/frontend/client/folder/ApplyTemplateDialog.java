package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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

		final boolean inheritOptionEnabled = "true".equals(Session.get().getInfo()
				.getConfig("gui.security.inheritoption"));

		final DynamicForm form = new DynamicForm();

		CheckboxItem inheritSecurity = new CheckboxItem();
		inheritSecurity.setName("inheritSecurity");
		inheritSecurity.setTitle(I18N.message("inheritparentsec"));
		form.setItems(inheritSecurity);

		SelectItem templateSelector = ItemFactory.newFolderTemplateSelector();

		if (inheritOptionEnabled)
			form.setItems(templateSelector, inheritSecurity);

		Button apply = new Button(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.setMargin(1);
		apply.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (!form.validate())
					return;

				final TreeNode selectedNode = (TreeNode) FolderNavigator.get().getSelectedRecord();
				final long folderId = Long.parseLong(selectedNode.getAttributeAsString("folderId"));
				long templateId = Long.parseLong(form.getValueAsString("foldertemplate"));

				FolderService.Instance.get().applyTemplate(folderId, templateId,
						inheritOptionEnabled && "true".equals(form.getValueAsString("inheritSecurity")),
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg0) {
								FolderNavigator.get().getTree().reloadChildren(selectedNode);
								GuiLog.info(I18N.message("templateapplied"), null);
								destroy();
							}
						});
			}
		});

		content.setMembers(form, apply);
		addItem(content);
	}
}