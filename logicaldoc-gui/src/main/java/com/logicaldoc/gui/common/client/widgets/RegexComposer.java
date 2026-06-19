package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.metadata.filler.RegexTesterDialog;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * A visual editor for regular expressions
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.3
 *
 */
public class RegexComposer extends Window {

    private final FormItem sourceItem;

    private final boolean inclusive;

    private final ChangedHandler changedHandler;

    public RegexComposer(TextItem sourceItem, boolean inclusive, ChangedHandler changedHandler) {
        this.sourceItem = sourceItem;
        this.inclusive = inclusive;
        this.changedHandler = changedHandler;

        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
        setTitle(I18N.message("regexcomposer"));
        setWidth(500);
        setHeight(250);

        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();

        initGUI();
    }

    private void initGUI() {

        DynamicForm form = new DynamicForm();
        form.setNumCols(1);
        form.setWidth100();

        CheckboxItem caseSensitive = ItemFactory.newCheckbox("casesensitive", "casesensitive");
        caseSensitive.setValue(false);

        TextItem primaryKeywords = ItemFactory.newTextItem("primarykeywords", null);
        primaryKeywords.setWrapTitle(false);
        primaryKeywords.setWidth(300);

        TextItem secondaryKeywords = ItemFactory.newTextItem("secondarykeywords", null);
        secondaryKeywords.setWrapTitle(false);
        secondaryKeywords.setWidth(300);

        TextItem separators = ItemFactory.newTextItem("separators", null);
        separators.setValue(":#");

        StaticTextItem regex = ItemFactory.newStaticTextItem("rregularexpression", null);
        regex.setWrap(false);

        ChangedHandler refresh = event -> regex
                .setValue(generateRegex(primaryKeywords.getValueAsString(), secondaryKeywords.getValueAsString(),
                        separators.getValueAsString(), Boolean.TRUE.equals(caseSensitive.getValue())));

        caseSensitive.addChangedHandler(refresh);
        primaryKeywords.addChangedHandler(refresh);
        secondaryKeywords.addChangedHandler(refresh);
        separators.addChangedHandler(refresh);

        ButtonItem test = new ButtonItem(I18N.message("regexrester"));
        test.setStartRow(true);

        test.addClickHandler(event -> {

            String generatedRegex = generateRegex(primaryKeywords.getValueAsString(),
                    secondaryKeywords.getValueAsString(), separators.getValueAsString(),
                    Boolean.TRUE.equals(caseSensitive.getValue()));

            TextItem tempRegex = new TextItem();
            tempRegex.setValue(generatedRegex);

            new RegexTesterDialog(tempRegex, inclusive, changedHandler).show();
        });

        ButtonItem apply = new ButtonItem(I18N.message("apply"));

        apply.addClickHandler(event -> {

            String generatedRegex = generateRegex(primaryKeywords.getValueAsString(),
                    secondaryKeywords.getValueAsString(), separators.getValueAsString(),
                    Boolean.TRUE.equals(caseSensitive.getValue()));

            if (sourceItem != null)
                sourceItem.setValue(generatedRegex);

            if (changedHandler != null) {
                changedHandler.onChanged(null);
            }

            destroy();
        });

        form.setFields(caseSensitive, primaryKeywords, secondaryKeywords, separators, regex, test, apply);

        VLayout layout = new VLayout();
        layout.setMargin(10);
        layout.setMembers(form);

        addItem(layout);
    }

    private String generateRegex(String primary, String secondary, String separators, boolean caseSensitive) {

        if (primary == null || primary.trim().isEmpty())
            return "";

        List<String> group1 = splitKeywords(primary);

        if (group1.isEmpty())
            return "";

        StringBuilder regex = new StringBuilder();

        if (!caseSensitive)
            regex.append("(?i)");

        // Primary keywords
        if (group1.size() == 1)
            regex.append(group1.get(0));
        else
            regex.append("(?:").append(String.join("|", group1)).append(")");

        // Secondary keywords
        List<String> group2 = splitKeywords(secondary);

        if (!group2.isEmpty()) {
            regex.append("\\s*");

            if (group2.size() == 1)
                regex.append(group2.get(0));
            else
                regex.append("(?:").append(String.join("|", group2)).append(")");
        }

        // Separators
        if (separators != null && !separators.trim().isEmpty())
            regex.append("\\s*[" + separators + "]?\\s*");
        else
            regex.append("\\s*");

        return regex.toString();
    }

    private List<String> splitKeywords(String text) {

        List<String> words = new ArrayList<>();

        if (text == null)
            return words;

        for (String word : text.split("[,\r\n]+")) {
            word = word.trim();

            if (!word.isEmpty())
                words.add(word);
        }
        return words;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((sourceItem == null) ? 0 : sourceItem.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RegexComposer other = (RegexComposer) obj;
        if (sourceItem == null) {
            if (other.sourceItem != null)
                return false;
        } else if (!sourceItem.equals(other.sourceItem))
            return false;
        return true;
    }
}