namespace Implementation.OutsideIn
{
    public interface IRule
    {
        bool AppliesTo(int number);
        string Result { get; }
    }
}