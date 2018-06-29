import io.vavr.collection.List;
import io.vavr.collection.Seq;

class CreateBundle {
    private final List<Bundle> bundles;
    private final Seq<BookSet> remainingSets;

    CreateBundle(List<Bundle> bundles, Seq<BookSet> sets) {
        this.bundles = bundles;
        this.remainingSets = sets;
    }

    CreateBundle(Seq<BookSet> bookSets) {
        this.bundles = List.empty();
        this.remainingSets = bookSets;
    }

    boolean isEmpty() {
        return remainingSets.isEmpty();
    }

    Seq<BookSet> getRemainingSets() {
        return remainingSets;
    }

    List<Bundle> getBundles() {
        return bundles;
    }

    CreateBundle next() {
        if (remainingSets.isEmpty()) {
            return this;
        }
        final var bundleToAdd = getHighestBundle(remainingSets, bundles);
        final var newBundles = addBundle(bundles, bundleToAdd);
        final var remainingSets = reduceBookCountIn(this.remainingSets);
        return new CreateBundle(newBundles, remainingSets);
    }

    private static Bundle getHighestBundle(Seq<BookSet> bookSets, List<Bundle> bundles) {
        final var numberOfDistinctBooks = bookSets.distinctBy(BookSet::getVolume).length();
        return bundles
                .filter(b -> b.has(numberOfDistinctBooks))
                .headOption()
                .getOrElse(() -> new Bundle(numberOfDistinctBooks, 0))
                .addBook();
    }

    private static List<Bundle> addBundle(List<Bundle> bundles, Bundle whichBundle) {
        return bundles.removeAll(b -> b.hasSameBookCount(whichBundle)).append(whichBundle);
    }

    private static Seq<BookSet> reduceBookCountIn(Seq<BookSet> remainingSets) {
        return remainingSets.map(BookSet::takeOneBook).filter(BookSet::notEmpty);
    }
}
