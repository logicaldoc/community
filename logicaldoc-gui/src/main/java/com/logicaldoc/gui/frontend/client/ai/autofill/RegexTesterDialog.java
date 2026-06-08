package com.logicaldoc.gui.frontend.client.ai.autofill;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * A visual editor for testing regular expressions
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.3
 */
public class RegexTesterDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private final TextItem originalRegexItem;

	private final boolean inclusive;

	public RegexTesterDialog(TextItem exclusionField, boolean inclusive) {
		this.originalRegexItem = exclusionField;
		this.inclusive = inclusive;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("regexrester"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		form.setWidth100();
		form.setHeight100();
		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem regexItem = ItemFactory.newTextItem("rregularexpression", originalRegexItem.getValueAsString());
		regexItem.setWidth(400);

		TextAreaItem sample = ItemFactory.newTextAreaItem("sampletoevaluate", "");
		sample.setWidth(400);
		sample.setHeight(100);

		StaticTextItem result = ItemFactory.newStaticTextItem("result", "");

		ButtonItem evaluate = new ButtonItem(I18N.message("evaluate"));
		evaluate.setStartRow(true);
		evaluate.addClickHandler(click -> {
			AutofillService.Instance.get().testRegex(sample.getValueAsString(), regexItem.getValueAsString(), inclusive,
					new DefaultAsyncCallback<String>() {

						@Override
						public void handleSuccess(String evaluation) {
							result.setValue(evaluation);
						}
					});
		});

		ButtonItem save = new ButtonItem(I18N.message("save"));
		save.addClickHandler(click -> originalRegexItem.setValue(regexItem.getValueAsString()));

		form.setItems(regexItem, sample, evaluate, result, save);

		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.setMembersMargin(5);
		layout.addMember(form);

		addItem(layout);
	}
}