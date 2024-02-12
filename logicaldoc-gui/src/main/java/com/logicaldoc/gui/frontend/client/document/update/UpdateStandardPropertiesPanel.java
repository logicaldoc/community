package com.logicaldoc.gui.frontend.client.document.update;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.DocumentDetailTab;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows document's standard properties available for bulk update.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class UpdateStandardPropertiesPanel extends DocumentDetailTab {
	private static final String LANGUAGE = "language";

	private DynamicForm form = new DynamicForm();

	private VLayout container = new VLayout();

	private VLayout formsContainer = new VLayout();

	private ValuesManager vm = new ValuesManager();

	protected boolean tagsInitialized = false;

	private MultiComboBoxItem tagItem;

	public UpdateStandardPropertiesPanel(GUIDocument document) {
		super(document, null);
		setWidth100();
		setHeight100();
		container.setWidth100();
		container.setMembersMargin(5);
		addMember(container);

		formsContainer.setWidth100();
		formsContainer.setMembersMargin(10);

		container.setMembers(formsContainer);
		refresh();
	}

	private void refresh() {
		vm.clearErrors(false);

		/*
		 * Prepare the second form for the tags
		 */
		prepareForm();
		formsContainer.addMember(form);
	}

	private void prepareForm() {
		if (Boolean.TRUE.equals(formsContainer.contains(form))) {
			formsContainer.removeMember(form);
			form.destroy();
		}

		form = new DynamicForm();
		form.setValuesManager(vm);

		List<FormItem> items = new ArrayList<>();

		SelectItem language = ItemFactory.newLanguageSelector(LANGUAGE, true, false);
		if (document.getLanguage() != null)
			language = ItemFactory.newLanguageSelector(LANGUAGE, false, false);
		language.setDisabled(!updateEnabled);
		language.setValue(document.getLanguage());
		items.add(language);

		ColorItem color = ItemFactory.newColorPickerItem(document.getColor(), true, changedHandler);
		color.setDisabled(!updateEnabled);
		items.add(color);

		if (Feature.enabled(Feature.TAGS)) {
			String mode = Session.get().getConfig("tag.mode");
			final DataSource ds = new TagsDS(null, true, document.getId(), null);

			tagItem = ItemFactory.newMultiComboBoxItem("tag", "tag", ds, document.getTags().toArray(new String[0]));
			tagItem.setPrompt(I18N.message("typeatag"));
			tagItem.setValueField("word");
			tagItem.setDisplayField("word");
			tagItem.setDisabled(!updateEnabled);

			final TextItem newTagItem = prepareNewTagItem(ds);

			final StaticTextItem tagsString = ItemFactory.newStaticTextItem("tags", "tag",
					Util.getTagsHTML(document.getTags()));
			tagsString.setEndRow(true);

			FormItemIcon editTags = new FormItemIcon();
			editTags.setPrompt(I18N.message("edittags"));
			editTags.setSrc("[SKIN]/actions/edit.png");
			editTags.setWidth(16);
			editTags.setHeight(16);
			editTags.addFormItemClickHandler((final FormItemIconClickEvent editTagsClick) -> {
				tagsString.setVisible(false);
				tagItem.setVisible(true);
				tagItem.setEndRow(true);
				if (items.contains(newTagItem)) {
					newTagItem.setVisible(true);
					newTagItem.setEndRow(true);
				}
				form.redraw();
			});
			tagsString.setIcons(editTags);

			if (updateEnabled) {
				items.add(tagsString);
				items.add(tagItem);
				tagItem.setVisible(false);
				newTagItem.setVisible(false);
				if ("free".equals(mode))
					items.add(newTagItem);
			}
		}

		form.setItems(items.toArray(new FormItem[0]));
	}

	private TextItem prepareNewTagItem(final DataSource ds) {
		final TextItem newTagItem = ItemFactory.newTextItem("newtag", "newtag", null);
		newTagItem.setRequired(false);
		newTagItem.addKeyPressHandler((KeyPressEvent newTagKeyPress) -> {
			if (Boolean.FALSE.equals(newTagItem.validate()) || newTagItem.getValue() == null
					|| newTagKeyPress.getKeyName() == null || !"enter".equalsIgnoreCase(newTagKeyPress.getKeyName()))
				return;

			String input = newTagItem.getValueAsString().trim();
			newTagItem.clearValue();

			if ("".equals(input))
				return;

			addTagsFromInput(input, ds);
		});
		return newTagItem;
	}

	private void addTagsFromInput(String input, final DataSource ds) {
		String[] tokens = input.split("\\,");
		int min = Integer.parseInt(Session.get().getConfig("tag.minsize"));
		int max = Integer.parseInt(Session.get().getConfig("tag.maxsize"));
		boolean containsInvalid = false;
		List<String> tags = new ArrayList<>();
		for (String token : tokens) {
			String t = token.trim();

			if (t.length() < min || t.length() > max) {
				containsInvalid = true;
				continue;
			}

			tags.add(t);

			// Put the new tag in the options
			Record rec = new Record();
			rec.setAttribute("index", t);
			rec.setAttribute("word", t);
			ds.addData(rec);

			// Add the old tags to the new ones
			String[] oldVal = tagItem.getValues();
			for (int i = 0; i < oldVal.length; i++)
				if (!tags.contains(oldVal[i]))
					tags.add(oldVal[i]);

			// Update the tag item and trigger the change
			tagItem.setValues((Object[]) tags.toArray(new String[0]));
			changedHandler.onChanged(null);
		}

		if (containsInvalid)
			SC.warn(I18N.message("sometagaddedbecauseinvalid"));
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean validate() {
		Map<String, Object> values = vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			document.setLanguage((String) values.get(LANGUAGE));
			document.setColor((String) values.get("color"));
			document.setTags(Arrays.asList(tagItem.getValues()));
		}
		return !vm.hasErrors();
	}
}