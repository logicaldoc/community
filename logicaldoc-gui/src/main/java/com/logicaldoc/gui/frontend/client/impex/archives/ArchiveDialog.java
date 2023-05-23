package com.logicaldoc.gui.frontend.client.impex.archives;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This is the form used to create a new Archive
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ArchiveDialog extends Window {

	private ValuesManager vm = new ValuesManager();

	private ExportArchivesList archivesPanel;

	public ArchiveDialog(ExportArchivesList archivesPanel) {
		this.archivesPanel = archivesPanel;

		addCloseClickHandler(event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("addarchive"));
		setWidth(320);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(true);

		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(280);
		form.setMargin(5);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem name = ItemFactory.newSimpleTextItem("name", null);
		name.setRequired(true);

		TextItem description = ItemFactory.newTextItem("description", null);
		description.setWidth(300);

		StaticTextItem creator = ItemFactory.newStaticTextItem("creator", Session.get().getUser().getFullName());

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> {
			vm.validate();
			if (Boolean.FALSE.equals(vm.hasErrors())) {
				GUIArchive archive = new GUIArchive();
				archive.setType(ArchiveDialog.this.archivesPanel.getArchivesType());
				archive.setName(vm.getValueAsString("name"));
				archive.setDescription(vm.getValueAsString("description"));
				archive.setCreatorId(Session.get().getUser().getId());
				archive.setCreatorName(Session.get().getUser().getFullName());
				archive.setMode(GUIArchive.MODE_EXPORT);

				ImpexService.Instance.get().save(archive, new AsyncCallback<GUIArchive>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						destroy();
					}

					@Override
					public void onSuccess(GUIArchive result) {
						destroy();
						// We can reload the archives list with the saved
						// archive, because all archives of the same list
						// have the same type
						ArchiveDialog.this.archivesPanel.refresh(result.getType(), false);
					}
				});
			}
		});

		form.setFields(creator, name, description, save);
		addItem(form);
	}
}