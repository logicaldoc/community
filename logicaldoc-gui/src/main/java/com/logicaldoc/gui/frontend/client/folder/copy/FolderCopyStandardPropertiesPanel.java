package com.logicaldoc.gui.frontend.client.folder.copy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.folder.FolderDetailTab;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.DoesntContainValidator;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows folder's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class FolderCopyStandardPropertiesPanel extends FolderDetailTab {

	private DynamicForm form1 = new DynamicForm();

	private DynamicForm form2 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private VLayout rows = new VLayout();

	protected MultiComboBoxItem tagItem = null;

	public FolderCopyStandardPropertiesPanel(GUIFolder folder) {
		super(folder, null);
		setWidth100();
		setHeight100();

		rows.setWidth100();
		rows.setMembersMargin(10);
		setMembers(rows);

		refresh();
	}

	private void refresh() {
		vm = new ValuesManager();

		if (form1 != null)
			form1.destroy();

		if (Boolean.TRUE.equals(contains(form1)))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.LEFT);

		TextItem name = ItemFactory.newTextItem("name", folder.getName());
		name.setWidth(200);
		name.setRequired(true);
		DoesntContainValidator validator = new DoesntContainValidator();
		validator.setSubstring("/");
		validator.setErrorMessage(I18N.message("invalidchar"));
		name.setValidators(validator);

		TextItem description = ItemFactory.newTextItem("description", folder.getDescription());
		description.setWidth(250);

		List<FormItem> items = new ArrayList<>();
		items.addAll(Arrays.asList(name, description));

		form1.setItems();

		rows.addMember(form1);

		/*
		 * Prepare the second form for the tags
		 */
		prepareRightForm();
	}

	private void prepareRightForm() {
		if (Boolean.TRUE.equals(rows.contains(form2))) {
			rows.removeMember(form2);
			form2.destroy();
		}

		form2 = new DynamicForm();
		form2.setAutoWidth();
		form2.setValuesManager(vm);

		if (Feature.enabled(Feature.TAGS))
			fillTagsForm();

		rows.addMember(form2);
	}

	private void fillTagsForm() {
		List<FormItem> items = new ArrayList<>();

		String mode = Session.get().getConfig("tag.mode");
		final TagsDS ds = new TagsDS(null, true, null, folder.getId());

		tagItem = ItemFactory.newTagsComboBoxItem("tag", "tag", ds, folder.getTags());
		tagItem.setEndRow(true);
		tagItem.setDisabled(!folder.isWrite());

		final TextItem newTagItem = ItemFactory.newTextItem("newtag", null);
		newTagItem.setRequired(false);
		newTagItem.setEndRow(true);
		newTagItem.addKeyPressHandler(event -> {
			if (Boolean.TRUE.equals(newTagItem.validate()) && newTagItem.getValue() != null
					&& event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName())) {
				String input = newTagItem.getValueAsString().trim();
				newTagItem.clearValue();

				if (!"".equals(input))
					addTagFromInput(ds, input);
			}
		});

		final StaticTextItem tagsString = ItemFactory.newStaticTextItem("tags", "tag",
				Util.getTagsHTML(folder.getTags()));
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

		tagsString.setIcons(editTags);

		items.add(tagsString);
		items.add(tagItem);
		tagItem.setVisible(false);
		newTagItem.setVisible(false);
		if ("free".equals(mode) && folder.isWrite())
			items.add(newTagItem);

		form2.setItems(items.toArray(new FormItem[0]));
	}

	private void addTagFromInput(final TagsDS ds, String input) {
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

	@Override
	public boolean validate() {
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			folder.setTags(Arrays.asList(tagItem.getValues()));
			folder.setDescription(vm.getValueAsString("description"));
			if (vm.getValueAsString("name") != null)
				folder.setName(vm.getValueAsString("name").replace("/", ""));
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