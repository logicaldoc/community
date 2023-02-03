package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.PickerIconName;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.validator.DoesntContainValidator;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows folder's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderStandardPropertiesPanel extends FolderDetailTab {

	private static final int DEFAULT_ITEM_WIDTH = 250;

	private DynamicForm form1 = new DynamicForm();

	private DynamicForm form2 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private HLayout columns = new HLayout();

	protected MultiComboBoxItem tagItem = null;

	public FolderStandardPropertiesPanel(GUIFolder folder, ChangedHandler changedHandler) {
		super(folder, changedHandler);
		setWidth100();
		setHeight100();

		columns.setWidth100();
		columns.setMembersMargin(10);
		setMembers(columns);

		refresh();
	}

	private void refresh() {
		vm = new ValuesManager();

		prepareForm1();

		StaticTextItem idItem = ItemFactory.newStaticTextItem("id", Long.toString(folder.getId()));
		if (folder.getFoldRef() != null)
			idItem.setTooltip(I18N.message("thisisalias") + ": " + folder.getFoldRef());

		TextItem name = prepareNameItem();

		FormItemIcon applyStorageToSubfolders = prepareApplyStorageToSubfoldersItem();

		FormItemIcon enforceStorage = prepareEnforceStorage();

		SelectItem storage = ItemFactory.newStorageSelector("storage", folder.getStorage());
		storage.setDisabled(!folder.hasPermission(Constants.PERMISSION_STORAGE));
		boolean storageVisible = folder.getFoldRef() == null && Feature.enabled(Feature.MULTI_STORAGE);
		storage.setVisible(storageVisible);
		if (folder.hasPermission(Constants.PERMISSION_STORAGE) && storageVisible) {
			storage.addChangedHandler(changedHandler);
			storage.setIcons(applyStorageToSubfolders, enforceStorage);
		}

		SpinnerItem maxVersions = prepareMaxVersionsItem();

		TextItem description = ItemFactory.newTextItem("description", folder.getDescription());
		description.setWidth(250);
		if (folder.hasPermission(Constants.PERMISSION_RENAME))
			description.addChangedHandler(changedHandler);
		else
			description.setDisabled(true);

		StaticTextItem creation = ItemFactory.newStaticTextItem("creation", "createdon",
				Util.textWithAvatar(folder.getCreatorId(), Util.padLeft(I18N.formatDate((Date) folder.getCreation())
						+ " " + I18N.message("by") + " " + folder.getCreator(), 40)));
		creation.setTooltip(
				I18N.formatDate((Date) folder.getCreation()) + " " + I18N.message("by") + " " + folder.getCreator());
		creation.setWidth(DEFAULT_ITEM_WIDTH);

		LinkItem pathItem = preparePathItem();

		LinkItem barcode = ItemFactory.newLinkItem("barcode", I18N.message("generatebarcode"));
		barcode.setTarget("_blank");
		barcode.setTitle(I18N.message("barcode"));
		barcode.setValue(GWT.getHostPageBaseURL() + "barcode?code=" + folder.getId() + "&width=400&height=150");

		final StaticTextItem documents = ItemFactory.newStaticTextItem("documents", 
				folder.getDocumentCount() > 0 ? Integer.toString(folder.getDocumentCount()) : "-");
		documents.setIconHSpace(2);
		documents.setIconWidth(16);
		documents.setIconHeight(16);
		documents.setWidth("1%");

		final StaticTextItem subfolders = ItemFactory.newStaticTextItem("folders", 
				folder.getSubfolderCount() > 0 ? Integer.toString(folder.getSubfolderCount()) : "-");
		subfolders.setIconHSpace(2);
		subfolders.setIconWidth(16);
		subfolders.setIconHeight(16);
		subfolders.setWidth("1%");

		addComputeStatsIcons(documents, subfolders);

		List<FormItem> items = new ArrayList<FormItem>();
		items.addAll(Arrays.asList(new FormItem[] { idItem, pathItem, name, description, storage, maxVersions, creation,
				documents, subfolders, barcode }));

		if (!Feature.enabled(Feature.BARCODES))
			items.remove(barcode);
		if (!Feature.enabled(Feature.MULTI_STORAGE))
			items.remove(storage);
		if (folder.isDefaultWorkspace())
			items.remove(name);

		form1.setItems(items.toArray(new FormItem[0]));

		columns.addMember(form1);

		/*
		 * Prepare the second form for the tags
		 */
		prepareRightForm();

	}

	private SpinnerItem prepareMaxVersionsItem() {
		SpinnerItem maxVersions = ItemFactory.newSpinnerItem("maxversions", folder.getMaxVersions());
		maxVersions.setWrapTitle(false);
		maxVersions.setDisabled(!folder.isWrite());
		boolean maxVersionsVisible = folder.isWorkspace() && folder.getFoldRef() == null;
		maxVersions.setVisible(maxVersionsVisible);
		if (folder.isWrite() && maxVersionsVisible)
			maxVersions.addChangedHandler(changedHandler);
		return maxVersions;
	}

	private void prepareForm1() {
		if (form1 != null)
			form1.destroy();

		if (Boolean.TRUE.equals(contains(form1)))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.LEFT);
	}

	private void addComputeStatsIcons(final StaticTextItem documents, final StaticTextItem subfolders) {
		PickerIcon computeStats = new PickerIcon(PickerIconName.REFRESH,
				(final FormItemIconClickEvent computeStatsClick) -> {
					computeStatsClick.getItem().setValue(I18N.message("computing") + "...");
					FolderService.Instance.get().computeStats(folder.getId(), new AsyncCallback<int[]>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(int[] stats) {
							folder.setDocumentCount(stats[0]);
							documents.setValue(Util.formatLong(stats[0]));
							folder.setSubfolderCount(stats[1]);
							subfolders.setValue(Util.formatLong(stats[1]));
						}
					});
				});
		computeStats.setPrompt(I18N.message("calculatestats"));

		documents.setIcons(computeStats);
		subfolders.setIcons(computeStats);
	}

	private LinkItem preparePathItem() {
		String path = folder.getPathExtended() != null ? folder.getPathExtended()
				: FolderNavigator.get().getPath(folder.getId());

		FormItemIcon copyPath = new FormItemIcon();
		copyPath.setPrompt(I18N.message("copypath"));
		copyPath.setSrc("[SKIN]/page_white_paste.png");
		copyPath.setWidth(16);
		copyPath.setHeight(16);
		copyPath.addFormItemClickHandler((final FormItemIconClickEvent copyPathClick) -> {
			LD.askForValue(I18N.message("path"), I18N.message("path"), path, new ValueCallback() {
				@Override
				public void execute(final String value) {
					// Nothing to do
				}
			});
			copyPathClick.cancel();
		});

		LinkItem pathItem = ItemFactory.newLinkItem("path", Util.padLeft(path, 150));
		pathItem.setTooltip(path);
		pathItem.setTitle(I18N.message("path"));
		pathItem.setValue(Util.displayURL(null, folder.getId()));
		pathItem.setWidth(400);
		pathItem.setIcons(copyPath);
		return pathItem;
	}

	private FormItemIcon prepareEnforceStorage() {
		FormItemIcon enforceStorage = new FormItemIcon();
		enforceStorage.setPrompt(I18N.message("enforcefilesintofolderstorage"));
		enforceStorage.setSrc("[SKIN]/data_into.png");
		enforceStorage.setWidth(16);
		enforceStorage.setHeight(16);
		enforceStorage.addFormItemClickHandler(new FormItemClickHandler() {
			public void onFormItemClick(final FormItemIconClickEvent event) {
				LD.ask(I18N.message("enforcementofstorage"),
						I18N.message("enforcefilesintofolderstorage") + ".\n" + I18N.message("doyouwanttoproceed"),
						(Boolean yes) -> {
							if (Boolean.TRUE.equals(yes)) {
								DocumentService.Instance.get().enforceFilesIntoFolderStorage(folder.getId(),
										new AsyncCallback<Void>() {

											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void v) {
												GuiLog.info(I18N.message("processstartedwillbenotified"));
											}
										});
							}
						});

				event.cancel();
			}
		});
		return enforceStorage;
	}

	private FormItemIcon prepareApplyStorageToSubfoldersItem() {
		FormItemIcon applyStorageToSubfolders = new FormItemIcon();
		applyStorageToSubfolders.setPrompt(I18N.message("applytosubfolders"));
		applyStorageToSubfolders.setSrc("[SKIN]/page_save.png");
		applyStorageToSubfolders.setWidth(16);
		applyStorageToSubfolders.setHeight(16);
		applyStorageToSubfolders
				.addFormItemClickHandler((final FormItemIconClickEvent applyStorageToSubfoldersClick) -> {
					LD.contactingServer();
					FolderService.Instance.get().applyStorage(folder.getId(), new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void v) {
							LD.clearPrompt();
						}
					});
					applyStorageToSubfoldersClick.cancel();
				});
		return applyStorageToSubfolders;
	}

	private TextItem prepareNameItem() {
		TextItem name = ItemFactory.newTextItem("name", folder.getName());
		name.setWidth(200);
		name.setRequired(true);
		DoesntContainValidator validator = new DoesntContainValidator();
		validator.setSubstring("/");
		validator.setErrorMessage(I18N.message("invalidchar"));
		name.setValidators(validator);
		if (folder.hasPermission(Constants.PERMISSION_RENAME) && folder.isWrite())
			name.addChangedHandler(changedHandler);
		else
			name.setDisabled(true);
		return name;
	}

	private void prepareRightForm() {
		if (Boolean.TRUE.equals(columns.contains(form2))) {
			columns.removeMember(form2);
			form2.destroy();
		}

		if (Feature.enabled(Feature.TAGS))
			fillRightForm();

		columns.addMember(form2);
	}

	private void fillRightForm() {
		form2 = new DynamicForm();
		form2.setAutoWidth();
		form2.setValuesManager(vm);
		List<FormItem> items = new ArrayList<FormItem>();

		String mode = Session.get().getConfig("tag.mode");
		final TagsDS ds = new TagsDS(null, true, null, folder.getId());

		tagItem = ItemFactory.newTagsComboBoxItem("tag", "tag", ds, (Object[]) folder.getTags());
		tagItem.setEndRow(true);
		tagItem.setDisabled(!folder.isWrite());
		if (folder.isWrite())
			tagItem.addChangedHandler(changedHandler);

		final TextItem newTagItem = prepareNewTagItem(ds);

		final StaticTextItem tagsString = ItemFactory.newStaticTextItem("tags", "tag",
				Util.getTagsHTML(folder.getTags()));
		tagsString.setEndRow(true);
		FormItemIcon editTags = new FormItemIcon();
		editTags.setPrompt(I18N.message("edittags"));
		editTags.setSrc("[SKIN]/actions/edit.png");
		editTags.setWidth(16);
		editTags.setHeight(16);
		editTags.addFormItemClickHandler((final FormItemIconClickEvent editTagClick) -> {
			tagsString.setVisible(false);
			tagItem.setVisible(true);
			tagItem.setEndRow(true);
			if (items.contains(newTagItem)) {
				newTagItem.setVisible(true);
				newTagItem.setEndRow(true);
			}
			form2.redraw();
		});

		if (folder.isWrite())
			tagsString.setIcons(editTags);

		items.add(tagsString);
		items.add(tagItem);
		tagItem.setVisible(false);
		newTagItem.setVisible(false);
		if ("free".equals(mode) && folder.isWrite())
			items.add(newTagItem);

		addApplyTagsToSubfoldersButton(items);

		form2.setItems(items.toArray(new FormItem[0]));
	}

	private TextItem prepareNewTagItem(final TagsDS ds) {
		final TextItem newTagItem = ItemFactory.newTextItem("newtag", null);
		newTagItem.setRequired(false);
		newTagItem.setEndRow(true);
		newTagItem.setDisabled(!folder.isWrite());
		newTagItem.addKeyPressHandler((KeyPressEvent event) -> {
			if (Boolean.TRUE.equals(newTagItem.validate()) && newTagItem.getValue() != null && event.getKeyName() != null
					&& "enter".equals(event.getKeyName().toLowerCase())) {
				String input = newTagItem.getValueAsString().trim();
				newTagItem.clearValue();

				if (!"".equals(input))
					onAddAddTagInput(ds, input);
			}
		});
		return newTagItem;
	}

	private void onAddAddTagInput(final TagsDS ds, String input) {
		// replace the escapes \, with a string so the
		// tokenizer will work propertly
		input = input.replace("\\,", "__comma__");

		String[] tokens = input.split("\\,");

		int min = Integer.parseInt(Session.get().getConfig("tag.minsize"));
		int max = Integer.parseInt(Session.get().getConfig("tag.maxsize"));
		boolean containsInvalid = false;
		List<String> tags = new ArrayList<String>();
		for (String token : tokens) {
			String t = token.trim();

			// Restore the commas inside the tag
			t = t.replace("__comma__", ",");

			if (t.length() < min || t.length() > max) {
				containsInvalid = true;
				continue;
			}

			tags.add(t);

			// Add the old tags to the new ones
			String[] oldVal = tagItem.getValues();
			for (int i = 0; i < oldVal.length; i++)
				if (!tags.contains(oldVal[i]))
					tags.add(oldVal[i]);

			// Put the new tag in the options
			Record rec = new Record();
			rec.setAttribute("index", t);
			rec.setAttribute("word", t);
			ds.addData(rec);
		}

		// Update the tag item and trigger the change
		tagItem.setValues((Object[]) tags.toArray(new String[0]));
		changedHandler.onChanged(null);

		if (containsInvalid)
			SC.warn(I18N.message("sometagaddedbecauseinvalid"));
	}

	private void addApplyTagsToSubfoldersButton(List<FormItem> items) {
		ButtonItem applyTags = new ButtonItem(I18N.message("applytosubfolders"));
		applyTags.setAutoFit(true);
		applyTags.setEndRow(true);
		applyTags.setColSpan(2);
		applyTags.setDisabled(!folder.isWrite());
		applyTags.addClickHandler((ClickEvent event) -> {
			LD.contactingServer();
			FolderService.Instance.get().applyTags(folder.getId(), new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void v) {
					LD.clearPrompt();
				}
			});
		});
		items.add(applyTags);
	}

	public boolean validate() {
		vm.validate();
		if (Boolean.TRUE.equals(vm.hasErrors()))
			return false;

		folder.setTags(tagItem.getValues());
		folder.setDescription(vm.getValueAsString("description"));
		if (vm.getValueAsString("name") != null)
			folder.setName(vm.getValueAsString("name").replace("/", ""));

		if (folder.hasPermission(Constants.PERMISSION_STORAGE))
			try {
				folder.setStorage(Integer.parseInt(vm.getValueAsString("storage")));
			} catch (Throwable t) {
				folder.setStorage(null);
			}

		if (folder.isWorkspace())
			try {
				folder.setMaxVersions(Integer.parseInt(vm.getValueAsString("maxversions")));
				if (folder.getMaxVersions() != null && folder.getMaxVersions() < 1)
					folder.setMaxVersions(null);
			} catch (Throwable t) {
				folder.setMaxVersions(null);
			}

		return !vm.hasErrors();
	}
}