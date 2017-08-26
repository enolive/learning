public interface Rule {
    boolean appliesTo(int input);

    String getResult();
}
