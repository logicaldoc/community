package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewTile;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.QRFormItemIcon;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentStandardPropertiesPanel extends DocumentDetailTab {

	private static final String FOLDER = "folder";

	private static final String COLOR = "color";

	private static final int DEFAULT_ITEM_WIDTH = 250;

	private DynamicForm form1 = new DynamicForm();

	private DynamicForm form2 = new DynamicForm();

	private VLayout container = new VLayout();

	private HLayout columns = new HLayout();

	private ValuesManager vm = new ValuesManager();

	protected MultiComboBoxItem tagItem = null;

	private Layout thumbnail = new HLayout();

	public DocumentStandardPropertiesPanel(final GUIDocument document, ChangedHandler changedHandler) {
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

		StaticTextItem id = prepareIdItem();

		StaticTextItem creation = ItemFactory.newStaticTextItem("creation", "createdon",
				Util.textWithAvatar(document.getCreatorId(), Util.padLeft(I18N.formatDate(document.getCreation()) + " "
						+ I18N.message("by") + " " + document.getCreator(), 40)));
		creation.setTooltip(
				I18N.formatDate(document.getCreation()) + " " + I18N.message("by") + " " + document.getCreator());

		StaticTextItem published = ItemFactory.newStaticTextItem("date", "publishedon",
				Util.textWithAvatar(document.getPublisherId(), Util.padLeft(
						I18N.formatDate(document.getDate()) + " " + I18N.message("by") + " " + document.getPublisher(),
						40)));
		published.setTooltip(
				I18N.formatDate(document.getDate()) + " " + I18N.message("by") + " " + document.getPublisher());

		StaticTextItem size = ItemFactory.newStaticTextItem("size",
				Util.formatSizeW7(document.getFileSize()) + " (" + Util.formatSizeBytes(document.getFileSize()) + ")");

		StaticTextItem pages = preparePagesItem();

		TextItem fileName = ItemFactory.newTextItem("filename", document.getFileName());
		fileName.addChangedHandler(changedHandler);
		fileName.setRequired(true);
		fileName.setWidth(DEFAULT_ITEM_WIDTH);

		fileName.setDisabled(!updateEnabled || !document.isRename());

		StaticTextItem wfStatus = prepareWorkflowItem();

		StaticTextItem version = prepareVersionItem();

		LinkItem folder = prepareFolderItem();

		ColorItem color = ItemFactory.newColorPickerItem(document.getColor(), true, changedHandler);
		color.setDisabled(!updateEnabled);

		StaticTextItem permaLink = preparePermaLink();

		TextItem customId = ItemFactory.newTextItem("customid", document.getCustomId());
		if (changedHandler != null)
			customId.addChangedHandler(changedHandler);
		customId.setDisabled(!updateEnabled || !document.isCustomid());
		customId.setVisible(Feature.enabled(Feature.CUSTOMID));

		TextItem revision = ItemFactory.newTextItem("revision", document.getRevision());
		if (changedHandler != null)
			revision.addChangedHandler(changedHandler);
		revision.setDisabled(!updateEnabled || !document.isRevision());
		revision.setVisible(Feature.enabled(Feature.REVISION));

		form1.setItems(id, customId, fileName, folder, size, pages, version, revision, creation, published, wfStatus,
				color, permaLink);

		columns.addMember(form1);

		/*
		 * Prepare the second form for the tags
		 */
		prepareRightForm();

		/*
		 * Prepare the tile
		 */
		if (document.getId() != 0L && document.isPreview()) {
			thumbnail = new PreviewTile(document.getId(), document.getFileName());
			columns.addMember(thumbnail);
		}
	}

	private StaticTextItem preparePermaLink() {
		String downloadUrl = Util.downloadURL(document.getId());
		String displayUrl = Util.displayURL(document.getId(), null);
		String perma = "<a href='" + downloadUrl + "' target='_blank'>" + I18N.message("download") + "</a> | "
				+ "<a href='" + displayUrl + "' target='_blank'>" + I18N.message("details") + "</a>";

		StaticTextItem permaLink = ItemFactory.newStaticTextItem("permalink", perma);

		if (Util.isCommercial()) {
			FormItemIcon qrCode = new FormItemIcon();
			qrCode.setPrompt(I18N.message("qrcode"));
			qrCode.setSrc("[SKIN]/icons/qrcode.png");
			qrCode.addFormItemClickHandler(click -> new PermaLinkDisplay(document.getId()).show());
			permaLink.setIcons(qrCode);
		}

		return permaLink;
	}

	private StaticTextItem prepareIdItem() {
		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(document.getId()));
		if (!Long.toString(document.getId()).equals(document.getCustomId())) {
			id.setTooltip(Long.toString(document.getId()) + " (" + document.getCustomId() + ")");
			id.setValue(Util.padLeft(Long.toString(document.getId()) + " (" + document.getCustomId() + ")", 35));
		}

		if (Util.isCommercial()) {
			StringBuilder sb = new StringBuilder("id: ");
			sb.append(document.getId());
			sb.append("_CR_filename: ");
			sb.append(document.getFileName());
			sb.append("_CR_version: ");
			sb.append(document.getVersion());
			sb.append("_CR_fileversion: ");
			sb.append(document.getFileVersion());
			sb.append("_CR_path: ");
			sb.append(document.getPathExtended());
			sb.append("/");
			sb.append(document.getFileName());
			id.setIcons(new QRFormItemIcon(sb.toString()));
		}

		if (document.getDocRef() != null)
			id.setTooltip(I18N.message("thisisalias") + ": " + document.getDocRef());
		return id;
	}

	private StaticTextItem prepareWorkflowItem() {
		StaticTextItem wfStatus = ItemFactory.newStaticTextItem("wfStatus", "workflowstatus",
				document.getWorkflowStatus());
		if (document.getWorkflowStatusDisplay() != null)
			wfStatus.setValue("<span style='color:" + document.getWorkflowStatusDisplay() + "'>"
					+ document.getWorkflowStatus() + "</span>");
		wfStatus.setVisible(Feature.enabled(Feature.WORKFLOW));
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
		LinkItem folder = ItemFactory.newLinkItem(FOLDER, FOLDER, Util.padLeft(path, 40),
				Util.displayURL(null, document.getFolder().getId()), path);
		folder.setTitle(I18N.message(FOLDER));
		folder.setTooltip(path);
		folder.setWrap(false);
		return folder;
	}

	private StaticTextItem preparePagesItem() {
		StaticTextItem pages = ItemFactory.newStaticTextItem("pages", Util.formatInt(document.getPages()));
		pages.setIconHSpace(2);
		pages.setWidth("1%");

		FormItemIcon countPages = new FormItemIcon();
		countPages.setSrc("[SKIN]/icons/arrows-rotate.png");
		countPages.setPrompt(I18N.message("countpages"));
		countPages.addFormItemClickHandler(click -> {
			click.getItem().setValue(I18N.message("computing") + "...");
			DocumentService.Instance.get().updatePages(document.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Integer docPages) {
					if (docPages != null) {
						document.setPages(docPages.intValue());
						pages.setValue(Util.formatInt(document.getPages()));
					}
				}
			});
		});

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

		tagItem = ItemFactory.newTagsComboBoxItem("tag", "tag", ds, document.getTags());
		tagItem.setEndRow(true);
		tagItem.setDisabled(!updateEnabled);
		tagItem.addChangedHandler(changedHandler);

		final TextItem newTagItem = prepareNewTagItem(ds);

		final StaticTextItem tagsString = ItemFactory.newStaticTextItem("tags", "tag",
				Util.getTagsHTML(document.getTags()));
		tagsString.setEndRow(true);

		FormItemIcon editTags = new FormItemIcon();
		editTags.setPrompt(I18N.message("edittags"));
		editTags.setSrc("[SKIN]/icons/pen-to-square.png");
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
					&& event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName())) {
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
			rating.addClickHandler(
					event -> DocumentService.Instance.get().getRating(document.getId(), new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(GUIRating rating) {
							if (rating != null) {
								RatingDialog dialog = new RatingDialog(document.getRating(), rating);
								dialog.show();
							}
						}
					}));
		rating.setDisabled(!updateEnabled);
		if (Menu.enabled(Menu.BRANDING))
			items.add(rating);
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean validate() {
		Map<String, Object> values = vm.getValues();
		if (Boolean.TRUE.equals(vm.validate())) {
			document.setFileName((String) values.get("filename"));
			document.setLanguage((String) values.get("language"));
			document.setColor((String) values.get(COLOR));
			document.setTags(Arrays.asList(tagItem.getValues()));
		}
		return !vm.hasErrors();
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