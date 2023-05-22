package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewTile;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.PickerIconName;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class StandardPropertiesPanel extends DocumentDetailTab {
	private static final String COLOR = "color";

	private static final int DEFAULT_ITEM_WIDTH = 250;

	private DynamicForm form1 = new DynamicForm();

	private DynamicForm form2 = new DynamicForm();

	private VLayout container = new VLayout();

	private HLayout columns = new HLayout();

	private ValuesManager vm = new ValuesManager();

	protected MultiComboBoxItem tagItem = null;

	private Layout thumbnail = new HLayout();

	public StandardPropertiesPanel(final GUIDocument document, ChangedHandler changedHandler) {
		super(document, changedHandler);
		setWidth100();
		setHeight100();
		container.setWidth100();
		container.setMembersMargin(5);
		addMember(container);

		columns.setWidth100();
		columns.setMembersMargin(10);

		container.setMembers(columns);
		refresh();
	}

	private void refresh() {
		prepareForm1();

		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(document.getId()));
		if (!Long.toString(document.getId()).equals(document.getCustomId())) {
			id.setTooltip(Long.toString(document.getId()) + " (" + document.getCustomId() + ")");
			id.setValue(Util.padLeft(Long.toString(document.getId()) + " (" + document.getCustomId() + ")", 35));
		}

		if (document.getDocRef() != null)
			id.setTooltip(I18N.message("thisisalias") + ": " + document.getDocRef());

		StaticTextItem creation = ItemFactory.newStaticTextItem("creation", "createdon",
				Util.textWithAvatar(document.getCreatorId(), Util.padLeft(I18N.formatDate((Date) document.getCreation())
						+ " " + I18N.message("by") + " " + document.getCreator(), 40)));
		creation.setTooltip(I18N.formatDate((Date) document.getCreation()) + " " + I18N.message("by") + " "
				+ document.getCreator());

		StaticTextItem published = ItemFactory.newStaticTextItem("date", "publishedon",
				Util.textWithAvatar(document.getPublisherId(), Util.padLeft(I18N.formatDate((Date) document.getDate())
						+ " " + I18N.message("by") + " " + document.getPublisher(), 40)));
		published.setTooltip(
				I18N.formatDate((Date) document.getDate()) + " " + I18N.message("by") + " " + document.getPublisher());

		StaticTextItem size = ItemFactory.newStaticTextItem("size",
				Util.formatSizeW7(document.getFileSize()) + " (" + Util.formatSizeBytes(document.getFileSize()) + ")");

		StaticTextItem pages = preparePagesItem();

		TextItem fileName = ItemFactory.newTextItem("filename", document.getFileName());
		fileName.addChangedHandler(changedHandler);
		fileName.setRequired(true);
		fileName.setWidth(DEFAULT_ITEM_WIDTH);
		fileName.setDisabled(!updateEnabled || !document.getFolder().isRename());

		StaticTextItem wfStatus = prepareWorkflowItem();

		StaticTextItem version = prepareVersionItem();

		LinkItem folder = prepareFolderItem();

		ColorItem color = ItemFactory.newColorItemPicker(document.getColor(), true, changedHandler);
		color.setDisabled(!updateEnabled);

		String downloadUrl = Util.downloadURL(document.getId());
		String displayUrl = Util.displayURL(document.getId(), null);
		String perma = "<a href='" + downloadUrl + "' target='_blank'>" + I18N.message("download") + "</a> | "
				+ "<a href='" + displayUrl + "' target='_blank'>" + I18N.message("details") + "</a>";

		StaticTextItem permaLink = ItemFactory.newStaticTextItem("permalink", perma);

		if (Feature.enabled(Feature.WORKFLOW))
			form1.setItems(id, fileName, folder, size, pages, version, creation, published, wfStatus, color, permaLink);
		else
			form1.setItems(id, fileName, folder, size, pages, version, creation, published, color, permaLink);

		columns.addMember(form1);

		/*
		 * Prepare the second form for the tags
		 */
		prepareRightForm();

		/*
		 * Prepare the tile
		 */
		if (document.getId() != 0L) {
			thumbnail = new PreviewTile(document.getId(), document.getFileName());
			columns.addMember(thumbnail);
		}
	}

	private StaticTextItem prepareWorkflowItem() {
		StaticTextItem wfStatus = ItemFactory.newStaticTextItem("wfStatus", "workflowstatus",
				document.getWorkflowStatus());
		if (document.getWorkflowStatusDisplay() != null)
			wfStatus.setValue("<span style='color:" + document.getWorkflowStatusDisplay() + "'>"
					+ document.getWorkflowStatus() + "</span>");
		return wfStatus;
	}

	private StaticTextItem prepareVersionItem() {
		StaticTextItem version = ItemFactory.newStaticTextItem("version", "fileversion",
				document.getFileVersion() + " (" + document.getVersion() + ")");
		version.setValue(version.getValue());
		String comment = document.getComment();
		if (comment != null && !"".equals(comment))
			version.setTooltip(comment);
		return version;
	}

	private void prepareForm1() {
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (thumbnail != null)
			thumbnail.destroy();

		if (Boolean.TRUE.equals(columns.contains(form1)))
			columns.removeMember(form1);

		if (Boolean.TRUE.equals(columns.contains(thumbnail)))
			columns.removeChild(thumbnail);

		form1 = new DynamicForm();
		form1.setNumCols(2);
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.LEFT);
		form1.setWidth(DEFAULT_ITEM_WIDTH);
	}

	private LinkItem prepareFolderItem() {
		String path = document.getPathExtended();

		FormItemIcon copyPath = new FormItemIcon();
		copyPath.setPrompt(I18N.message("copypath"));
		copyPath.setSrc("[SKIN]/page_white_paste.png");
		copyPath.setWidth(16);
		copyPath.setHeight(16);
		copyPath.addFormItemClickHandler(event -> {
			LD.askForValue(I18N.message("path"), I18N.message("path"), path, new ValueCallback() {
				@Override
				public void execute(final String value) {
					// Nothing to do
				}
			});
			event.cancel();
		});

		LinkItem folder = ItemFactory.newLinkItem("folder", Util.padLeft(path, 40));
		folder.setTitle(I18N.message("folder"));
		folder.setValue(Util.displayURL(null, document.getFolder().getId()));
		folder.setTooltip(document.getPathExtended());
		folder.setWrap(false);
		folder.setWidth(DEFAULT_ITEM_WIDTH);
		folder.setIcons(copyPath);
		return folder;
	}

	private StaticTextItem preparePagesItem() {
		StaticTextItem pages = ItemFactory.newStaticTextItem("pages", Util.formatInt(document.getPages()));
		pages.setIconHSpace(2);
		pages.setIconWidth(16);
		pages.setIconHeight(16);
		pages.setWidth("1%");
		PickerIcon countPages = new PickerIcon(PickerIconName.REFRESH, (final FormItemIconClickEvent event) -> {
			event.getItem().setValue(I18N.message("computing") + "...");
			DocumentService.Instance.get().updatePages(document.getId(), new AsyncCallback<Integer>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Integer docPages) {
					if (docPages != null) {
						document.setPages(docPages.intValue());
						pages.setValue(Util.formatInt(document.getPages()));
					}
				}
			});
		});
		countPages.setPrompt(I18N.message("countpages"));
		if (document.getId() != 0)
			pages.setIcons(countPages);
		return pages;
	}

	private void prepareRightForm() {
		if (Boolean.TRUE.equals(columns.contains(form2))) {
			columns.removeMember(form2);
			form2.destroy();
		}

		form2 = new DynamicForm();
		form2.setValuesManager(vm);

		List<FormItem> items = new ArrayList<>();

		addRating(items);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, false);
		language.setEndRow(true);
		language.addChangedHandler(changedHandler);
		language.setDisabled(!updateEnabled);
		language.setValue(document.getLanguage());
		items.add(language);

		if (Feature.enabled(Feature.TAGS)) {
			addTags(items);
		}

		form2.setItems(items.toArray(new FormItem[0]));
		columns.addMember(form2);
	}

	private void addTags(List<FormItem> items) {
		String mode = Session.get().getConfig("tag.mode");
		final TagsDS ds = new TagsDS(null, true, document.getId(), null);

		tagItem = ItemFactory.newTagsComboBoxItem("tag", "tag", ds, (Object[]) document.getTags());
		tagItem.setEndRow(true);
		tagItem.setDisabled(!updateEnabled);
		tagItem.addChangedHandler(changedHandler);

		final TextItem newTagItem = prepareNewTagItem(ds);

		final StaticTextItem tagsString = ItemFactory.newStaticTextItem("tags", "tag",
				Util.getTagsHTML(document.getTags()));
		tagsString.setEndRow(true);

		FormItemIcon editTags = new FormItemIcon();
		editTags.setPrompt(I18N.message("edittags"));
		editTags.setSrc("[SKIN]/actions/edit.png");
		editTags.setWidth(16);
		editTags.setHeight(16);
		editTags.addFormItemClickHandler(event -> {
			tagsString.setVisible(false);
			tagItem.setVisible(true);
			tagItem.setEndRow(true);
			if (items.contains(newTagItem)) {
				newTagItem.setVisible(true);
				newTagItem.setEndRow(true);
			}
			form2.redraw();
		});

		if (updateEnabled)
			tagsString.setIcons(editTags);

		items.add(tagsString);
		items.add(tagItem);
		tagItem.setVisible(false);
		newTagItem.setVisible(false);
		if ("free".equals(mode) && updateEnabled)
			items.add(newTagItem);
	}

	private TextItem prepareNewTagItem(final TagsDS ds) {
		final TextItem newTagItem = ItemFactory.newTextItem("newtag", null);
		newTagItem.setEndRow(true);
		newTagItem.setRequired(false);
		newTagItem.addKeyPressHandler(event -> {
			if (Boolean.TRUE.equals(newTagItem.validate()) && newTagItem.getValue() != null
					&& event.getKeyName() != null && "enter".equals(event.getKeyName().toLowerCase())) {
				String input = newTagItem.getValueAsString().trim();
				newTagItem.clearValue();

				if (!"".equals(input)) {
					onAddNewTag(ds, input);
				}
			}
		});
		return newTagItem;
	}

	private void onAddNewTag(final TagsDS ds, String input) {
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
			SC.warn(I18N.message("sometagaddedbecauseinvalid", "" + min, "" + max));
	}

	private void addRating(List<FormItem> items) {
		StaticTextItem rating = ItemFactory.newStaticTextItem("rating",
				document.getRating() > 0 ? DocUtil.getRatingIcon(document.getRating())
						: I18N.message("ratethisdocument"));
		rating.setEndRow(true);
		if (updateEnabled)
			rating.addClickHandler((ClickEvent event) -> {
				DocumentService.Instance.get().getRating(document.getId(), new AsyncCallback<GUIRating>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIRating rating) {
						if (rating != null) {
							RatingDialog dialog = new RatingDialog(document.getRating(), rating);
							dialog.show();
						}
					}
				});
			});
		rating.setDisabled(!updateEnabled);
		if (Menu.enabled(Menu.BRANDING))
			items.add(rating);
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		if (Boolean.TRUE.equals(vm.validate())) {
			document.setFileName((String) values.get("filename"));
			document.setLanguage((String) values.get("language"));
			document.setColor((String) values.get(COLOR));
			document.setTags(tagItem.getValues());
		}
		return !vm.hasErrors();
	}
}