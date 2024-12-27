package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.QRFormItemIcon;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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

		StaticTextItem idItem = prepareIdItem();

		TextItem name = prepareNameItem();

		FormItemIcon applyStoreToSubfolders = prepareApplyStoreToSubfoldersItem();

		FormItemIcon enforceStore = prepareEnforceStore();

		SelectItem store = ItemFactory.newStoreSelector("store", folder.getStore());
		store.setDisabled(!folder.hasPermission(GUIAccessControlEntry.PERMISSION_STORE));
		boolean storeVisible = folder.getFoldRef() == null && Feature.enabled(Feature.MULTI_STORE);
		store.setVisible(storeVisible);
		if (folder.hasPermission(GUIAccessControlEntry.PERMISSION_STORE) && storeVisible) {
			store.addChangedHandler(changedHandler);
			store.setIcons(applyStoreToSubfolders, enforceStore);
		}

		SpinnerItem maxVersions = prepareMaxVersionsItem();

		TextItem description = ItemFactory.newTextItem("description", folder.getDescription());
		description.setWidth(250);
		if (folder.hasPermission(GUIAccessControlEntry.PERMISSION_RENAME))
			description.addChangedHandler(changedHandler);
		else
			description.setDisabled(true);

		StaticTextItem creation = ItemFactory.newStaticTextItem("creation", "createdon",
				Util.textWithAvatar(folder.getCreatorId(), Util.padLeft(
						I18N.formatDate(folder.getCreation()) + " " + I18N.message("by") + " " + folder.getCreator(),
						40)));
		creation.setTooltip(
				I18N.formatDate(folder.getCreation()) + " " + I18N.message("by") + " " + folder.getCreator());
		creation.setWidth(DEFAULT_ITEM_WIDTH);

		LinkItem pathItem = preparePathItem();

		final StaticTextItem documents = ItemFactory.newStaticTextItem("documents",
				folder.getDocumentCount() > 0 ? Util.formatLong(folder.getDocumentCount()) : "-");
		documents.setIconHSpace(2);
		documents.setWidth("1%");

		final StaticTextItem subfolders = ItemFactory.newStaticTextItem("folders",
				folder.getSubfolderCount() > 0 ? Util.formatLong(folder.getSubfolderCount()) : "-");
		subfolders.setIconHSpace(2);
		subfolders.setWidth("1%");

		final StaticTextItem size = ItemFactory.newStaticTextItem("size",
				folder.getSize() > 0 ? Util.formatSize(folder.getSize()) : "-");
		size.setIconHSpace(2);
		size.setWidth("1%");

		addComputeStatsIcons(documents, subfolders, size);

		List<FormItem> items = new ArrayList<>();
		items.addAll(Arrays.asList(idItem, pathItem, name, description, store, maxVersions, creation, documents,
				subfolders, size));
		if (!Feature.enabled(Feature.MULTI_STORE))
			items.remove(store);
		if (folder.isDefaultWorkspace())
			items.remove(name);

		form1.setItems(items.toArray(new FormItem[0]));

		columns.addMember(form1);

		/*
		 * Prepare the second form for the tags
		 */
		prepareRightForm();

		columns.addMember(new FolderTile(folder, changedHandler));
	}

	private StaticTextItem prepareIdItem() {
		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(folder.getId()));

		StringBuilder sb = new StringBuilder("id: ");
		sb.append(folder.getId());
		sb.append("_CR_name: ");
		sb.append(folder.getName());
		sb.append("_CR_path: ");
		sb.append(folder.getPathExtended());

		if (Util.isCommercial())
			id.setIcons(new QRFormItemIcon(sb.toString()));

		if (folder.getFoldRef() != null)
			id.setTooltip(I18N.message("thisisalias") + ": " + folder.getFoldRef());

		return id;
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

	private void addComputeStatsIcons(StaticTextItem documents, StaticTextItem subfolders, StaticTextItem size) {
		FormItemIcon computeStats = new FormItemIcon();
		computeStats.setPrompt(I18N.message("calculatestats"));
		computeStats.setSrc("[SKIN]/arrows-rotate.svg");
		computeStats.addFormItemClickHandler(click -> {
			click.getItem().setValue(I18N.message("computing") + "...");
			FolderService.Instance.get().computeStats(folder.getId(), new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);

				}

				@Override
				public void onSuccess(List<Long> stats) {
					folder.setDocumentCount(stats.get(0));
					documents.setValue(Util.formatLong(stats.get(0)));
					folder.setSubfolderCount(stats.get(1));
					subfolders.setValue(Util.formatLong(stats.get(1)));
					folder.setSize(stats.get(2));
					size.setValue(Util.formatSize(stats.get(2)));
				}
			});
		});

		documents.setIcons(computeStats);
		subfolders.setIcons(computeStats);
		size.setIcons(computeStats);
	}

	private LinkItem preparePathItem() {
		String path = folder.getPathExtended() != null ? folder.getPathExtended()
				: FolderNavigator.get().getPath(folder.getId());
		LinkItem pathItem = ItemFactory.newLinkItem("path", "path", Util.padLeft(path, 40),
				Util.displayURL(null, folder.getId()), path);

		pathItem.setTooltip(path);
		pathItem.setWrap(false);
		return pathItem;
	}

	private FormItemIcon prepareEnforceStore() {
		FormItemIcon enforceStore = new FormItemIcon();
		enforceStore.setPrompt(I18N.message("enforcefilesintofolderstorage"));
		enforceStore.setSrc("[SKIN]/boxes-stacked.svg");
		enforceStore.addFormItemClickHandler(event -> {
			LD.ask(I18N.message("enforcementofstorage"),
					I18N.message("enforcefilesintofolderstorage") + ".\n" + I18N.message("doyouwanttoproceed"), yes -> {
						if (Boolean.TRUE.equals(yes)) {
							LD.contactingServer();
							DocumentService.Instance.get().enforceFilesIntoFolderStore(folder.getId(),
									new AsyncCallback<>() {

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
						}
					});

			event.cancel();
		});
		return enforceStore;
	}

	private FormItemIcon prepareApplyStoreToSubfoldersItem() {
		FormItemIcon applyStoreToSubfolders = new FormItemIcon();
		applyStoreToSubfolders.setPrompt(I18N.message("applytosubfolders"));
		applyStoreToSubfolders.setSrc("[SKIN]/clone.svg");
		applyStoreToSubfolders.addFormItemClickHandler(applyStoreToSubfoldersClick -> {
			LD.contactingServer();
			FolderService.Instance.get().applyStore(folder.getId(), new AsyncCallback<>() {

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
			applyStoreToSubfoldersClick.cancel();
		});
		return applyStoreToSubfolders;
	}

	private TextItem prepareNameItem() {
		TextItem name = ItemFactory.newTextItem("name", folder.getName());
		name.setWidth(200);
		name.setRequired(true);
		DoesntContainValidator validator = new DoesntContainValidator();
		validator.setSubstring("/");
		validator.setErrorMessage(I18N.message("invalidchar"));
		name.setValidators(validator);
		if (folder.hasPermission(GUIAccessControlEntry.PERMISSION_RENAME) && folder.isWrite())
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
		List<FormItem> items = new ArrayList<>();

		String mode = Session.get().getConfig("tag.mode");
		final TagsDS ds = new TagsDS(null, true, null, folder.getId());

		tagItem = ItemFactory.newTagsComboBoxItem("tag", "tag", ds, folder.getTags());
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
		editTags.setSrc("[SKIN]/pen-to-square.svg");
		editTags.addFormItemClickHandler(editTagClick -> {
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
		newTagItem.addKeyPressHandler(event -> {
			if (Boolean.TRUE.equals(newTagItem.validate()) && newTagItem.getValue() != null
					&& event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName())) {
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
		List<String> tags = new ArrayList<>();
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
		applyTags.addClickHandler(event -> {
			LD.contactingServer();
			FolderService.Instance.get().applyTags(folder.getId(), new AsyncCallback<>() {

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

	@Override
	public boolean validate() {
		vm.validate();
		if (Boolean.TRUE.equals(vm.hasErrors()))
			return false;

		folder.setTags(Arrays.asList(tagItem.getValues()));
		folder.setDescription(vm.getValueAsString("description"));
		if (vm.getValueAsString("name") != null)
			folder.setName(vm.getValueAsString("name").replace("/", ""));

		if (folder.hasPermission(GUIAccessControlEntry.PERMISSION_STORE))
			try {
				folder.setStore(Integer.parseInt(vm.getValueAsString("store")));
			} catch (Exception t) {
				folder.setStore(null);
			}

		if (folder.isWorkspace())
			try {
				folder.setMaxVersions(Integer.parseInt(vm.getValueAsString("maxversions")));
				if (folder.getMaxVersions() != null && folder.getMaxVersions() < 1)
					folder.setMaxVersions(null);
			} catch (Exception t) {
				folder.setMaxVersions(null);
			}

		return !vm.hasErrors();
	}
}