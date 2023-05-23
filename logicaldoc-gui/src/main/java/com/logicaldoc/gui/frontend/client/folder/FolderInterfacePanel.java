package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;

/**
 * Shows the folder's user interface settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class FolderInterfacePanel extends FolderDetailTab {

	private static final String NOTCUSTOMIZED = "notcustomized";

	private static final String CUSTOMIZED = "customized";

	private static final String DOCSGRIDLAYOUT = "docsgridlayout";

	private static final String COLOR = "color";

	private static final String POSITION = "position";

	private FolderTile tile;

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private FormItemIcon applyToSubFolders = new FormItemIcon();

	public FolderInterfacePanel(GUIFolder folder, ChangedHandler changedHandler) {
		super(folder, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(20);

		try {
			refresh();
		} catch (Throwable t) {
			GuiLog.error(t.getMessage(), null, null);
			SC.warn(t.getMessage());
		}
	}

	private void refresh() {
		vm = new ValuesManager();

		if (form != null)
			form.destroy();

		if (Boolean.TRUE.equals(contains(form)))
			removeChild(form);

		if (Boolean.TRUE.equals(contains(tile)))
			removeChild(tile);

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		SpinnerItem position = ItemFactory.newSpinnerItem(POSITION, folder.getPosition());

		ColorItem color = ItemFactory.newColorItemPicker(folder.getColor(), true, changedHandler);

		StaticTextItem docsGrid = ItemFactory.newStaticTextItem("grid", DOCSGRIDLAYOUT,
				folder.getGrid() != null && !folder.getGrid().isEmpty() ? I18N.message(CUSTOMIZED)
						: I18N.message(NOTCUSTOMIZED));

		FormItemIcon saveCurrentLayout = prepareSaveCurrentLayout();

		FormItemIcon editLayout = prepareEditLayout();

		applyToSubFolders = new FormItemIcon();
		applyToSubFolders.setSrc("[SKIN]/download.png");
		applyToSubFolders.setPrompt(I18N.message("applytosubfolders"));
		applyToSubFolders.setWidth(12);
		applyToSubFolders.setHeight(12);
		applyToSubFolders.addFormItemClickHandler(event -> {
			LD.contactingServer();
			FolderService.Instance.get().applyGridLayout(folder.getId(), new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg0) {
					LD.clearPrompt();
					GuiLog.info(I18N.message("appliedgridonsubfolders"));
				}
			});
		});

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, event -> {
			folder.setGrid(null);
			event.getItem().setValue(I18N.message(NOTCUSTOMIZED));
			applyToSubFolders.setDisabled(true);
			if (changedHandler != null)
				changedHandler.onChanged(null);
		});
		clear.setWidth(12);
		clear.setHeight(12);

		docsGrid.setIcons(saveCurrentLayout, applyToSubFolders, editLayout, clear);

		form.setItems(position, color, docsGrid);
		addMember(form);

		tile = new FolderTile(folder, changedHandler);
		addMember(tile);

		if (folder.isWrite()) {
			if (changedHandler != null) {
				color.addChangedHandler(changedHandler);
				position.addChangedHandler(changedHandler);
				docsGrid.addChangedHandler(changedHandler);
			} else
				docsGrid.setHidden(true);
		} else {
			color.setDisabled(true);
			position.setDisabled(true);
			docsGrid.setDisabled(true);
		}
	}

	private FormItemIcon prepareEditLayout() {
		FormItemIcon editLayout = new FormItemIcon();
		editLayout.setSrc("[SKIN]/paste.gif");
		editLayout.setPrompt(I18N.message("editlayout"));
		editLayout.setWidth(12);
		editLayout.setHeight(12);
		editLayout.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			TextAreaItem textArea = ItemFactory.newTextAreaItem(DOCSGRIDLAYOUT, null);
			textArea.setHeight(300);
			LD.askForValue(I18N.message(DOCSGRIDLAYOUT), I18N.message(DOCSGRIDLAYOUT),
					folder.getGrid() != null ? folder.getGrid() : "", textArea, 400, (final String value) -> {
						vm.setValue("grid", (value != null && !value.trim().isEmpty()) ? I18N.message(CUSTOMIZED)
								: I18N.message(NOTCUSTOMIZED));

						folder.setGrid(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);

					});
			event.cancel();
		});
		return editLayout;
	}

	private FormItemIcon prepareSaveCurrentLayout() {
		FormItemIcon saveCurrentLayout = new FormItemIcon();
		saveCurrentLayout.setSrc("[SKIN]/paste.gif");
		saveCurrentLayout.setPrompt(I18N.message("savecurrentfolderlayout"));
		saveCurrentLayout.setWidth(12);
		saveCurrentLayout.setHeight(12);
		saveCurrentLayout.addFormItemClickHandler((FormItemIconClickEvent event) -> {
			String gridState = DocumentsPanel.get().getDocsGridViewState();
			if (gridState != null) {
				folder.setGrid(gridState);
				event.getItem().setValue(I18N.message(CUSTOMIZED));
				applyToSubFolders.setDisabled(true);
				if (changedHandler != null)
					changedHandler.onChanged(null);
			}
		});
		return saveCurrentLayout;
	}

	public boolean validate() {
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			folder.setPosition(
					vm.getValueAsString(POSITION) != null ? Integer.parseInt(vm.getValueAsString(POSITION)) : 1);
			folder.setColor(vm.getValueAsString(COLOR));

			if (vm.getValueAsString("grid") == null || vm.getValueAsString("grid").isEmpty()
					|| I18N.message(NOTCUSTOMIZED).equals(vm.getValueAsString("grid")))
				folder.setGrid(null);
		}
		return !vm.hasErrors();
	}
}