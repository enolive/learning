public class Rule {
    private final int arabic;
    private final String roman;

    public Rule(int arabic, String roman) {
        this.arabic = arabic;
        this.roman = roman;
    }

    public int getArabic() {
        return arabic;
    }

    public String getRoman() {
        return roman;
    }
}
