package com.logicaldoc.core.store;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;

import org.junit.Test;

public class StoreResourceTest {

    // Legitimate names must be unchanged (existing files must still be found)
    @Test
    public void testFileVersionOnly() {
        assertEquals("1.0", resource("1.0", null).name());
        assertEquals("12.345", resource("12.345", null).name());
    }

    @Test
    public void testStandardSuffixes() {
        assertEquals("1.0-conversion.pdf",
                StoreResource.builder().docId(1L).fileVersion("1.0").suffixPdfConversion().build().name());
        assertEquals("1.0-thumb.png",
                StoreResource.builder().docId(1L).fileVersion("1.0").suffixThumbnail().build().name());
        assertEquals("1.0-tile.png", StoreResource.builder().docId(1L).fileVersion("1.0").suffixTile().build().name());
        assertEquals("1.0-mobile.png",
                StoreResource.builder().docId(1L).fileVersion("1.0").suffixMobile().build().name());
        assertEquals("1.0-summary.html",
                StoreResource.builder().docId(1L).fileVersion("1.0").suffixSummary().build().name());
    }

    @Test
    public void testSuffixWithDashes() {
        assertEquals("2.1-page-1.png", resource("2.1", "page-1.png").name());
    }

    @Test
    public void testBuilderName() {
        StoreResource res = StoreResource.builder().docId(1L).name("1.0-conversion.pdf").build();
        assertEquals("1.0", res.getFileVersion());
        assertEquals("conversion.pdf", res.getSuffix());
        assertEquals("1.0-conversion.pdf", res.name());
    }

    // Separators are stripped: the name can never contain / or \
    @Test
    public void testSlashIsStripped() {
        assertEquals("1.0-ab", resource("1.0", "a/b").name());
    }

    @Test
    public void testBackslashIsStripped() {
        assertEquals("1.0-ab", resource("1.0", "a\\b").name());
    }

    @Test
    public void testNameNeverContainsSeparators() {
        String[] inputs = { "a/b", "a\\b", "/abs", "\\abs", "c:\\x", "x/y\\z" };
        for (String input : inputs) {
            String name = resource("1.0", input).name();
            assertFalse("Separator in name: " + name, name.contains("/") || name.contains("\\"));
        }
    }

    // Traversal attempts in the suffix must be rejected
    @Test
    public void testPlainDotDotSuffix() {
        assertRejected("1.0", "..");
        assertRejected("1.0", "../x");
        assertRejected("1.0", "..\\x");
        assertRejected("1.0", "..\\..\\..\\x");
    }

    @Test
    public void testReformedDotDotSuffix() {
        // Old code: "./." has no "..", then stripping "/" re-formed ".."
        assertRejected("1.0", "./.");
        assertRejected("1.0", ".\\.");
        assertRejected("1.0", "./.\\./.\\x");
        assertRejected("1.0", "./.\\./.\\./.\\windows\\win.ini");
    }

    @Test
    public void testNestedDotDotSuffix() {
        // Old code: a single replace of ".." on "...." or ".../" left ".."
        // behind
        assertRejected("1.0", "....");
        assertRejected("1.0", ".../x");
        assertRejected("1.0", "...");
    }

    @Test
    public void testEncodedLookingDotDotSuffix() {
        // Characters stripped in between must not glue dots back together
        assertRejected("1.0", ".%.");
        assertRejected("1.0", ". .");
        assertRejected("1.0", ".:.");
    }

    // Traversal attempts in the fileVersion must be rejected too
    @Test
    public void testTraversalInFileVersion() {
        assertRejected("..", null);
        assertRejected("./.", null);
        assertRejected(".", null);
        assertRejected("..\\..\\x", null);
        assertRejected("./.\\./.\\x", null);
    }

    @Test
    public void testFileVersionMadeOnlyOfInvalidCharacters() {
        // Everything is stripped, leaving an empty name
        assertRejected("///", null);
        assertRejected("\\\\", null);
    }
    
    private static StoreResource resource(String fileVersion, String suffix) {
        return StoreResource.builder().docId(1L).fileVersion(fileVersion).suffix(suffix).build();
    }

    private static void assertRejected(String fileVersion, String suffix) {
        StoreResource res = resource(fileVersion, suffix);
        assertThrows("Expected rejection for fileVersion=" + fileVersion + " suffix=" + suffix,
                IllegalArgumentException.class, res::name);
    }
}
