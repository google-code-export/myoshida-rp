using System;
using System.Windows.Markup;


namespace MarkupExtensionSample
{
    [MarkupExtensionReturnTypeAttribute(typeof(string))]
    public class TransExtension : MarkupExtension
    {

        string _key;

        public TransExtension(string key)
        {
            _key = key;
        }

        const string NotFoundError = "#StringNotFound#";

        public override object ProvideValue(IServiceProvider serviceProvider)
        {
            if (string.IsNullOrEmpty(_key))
                return NotFoundError;

            return Properties.Resources.ResourceManager.GetString(_key) ?? NotFoundError;
        }

    }
}
